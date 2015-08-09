module Solver where

import Prelude ()
import MyPrelude
import Debug.Trace


import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Diagrams.Prelude as D
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Data.DList as DL
import System.IO.Unsafe
import Unsafe.Coerce

import Data.Tuple
import Linear hiding (transpose)

import Game
import Optimizer

data Rotation = RE | RSE | RSW | RW | RNW | RNE
              deriving (Eq, Ord, Enum, Bounded, Show, Generic)
instance NFData Rotation
instance Hashable Rotation

{-# INLINE rotateCW #-}
{-# INLINE rotateCCW #-}
rotateCW, rotateCCW :: Rotation -> Rotation
rotateCW RE  = RSE
rotateCW RSE = RSW
rotateCW RSW = RW
rotateCW RW  = RNW
rotateCW RNW = RNE
rotateCW RNE = RE

rotateCCW RE  = RNE
rotateCCW RNE = RNW
rotateCCW RNW = RW
rotateCCW RW  = RSW
rotateCCW RSW = RSE
rotateCCW RSE = RE

-- A point with base (E, SE)
type ESE = V2 Int
{-# INLINE rotESE #-}
rotESE :: Rotation -> ESE -> ESE
rotESE RE (V2 x y) = V2 x y
rotESE RSE (V2 x y) = V2 (-y) (x+y)
rotESE RSW (V2 x y) = V2 (-x-y) x
rotESE RW (V2 x y) = V2 (-x) (-y)
rotESE RNW (V2 x y) = V2 y (-x-y)
rotESE RNE (V2 x y) = V2 (x+y) (-x)

{-# INLINE toESE #-}
{-# INLINE fromESE #-}
toESE :: (Int, Int) -> ESE
toESE (x, y) = V2 (x - y `div` 2) y
fromESE :: ESE -> (Int, Int)
fromESE (V2 x y) = (x + y `div` 2, y)

-- Position of the (0, 0) point of the unit (in ESE coords) and unit rotation
type Position = (ESE, Rotation)

-- UnitTransition a ~ Command -> a
-- ordre : MoveW, MoveE, MoveSW, MoveSE, RotateCW, RotateCCW
data UnitTransition a = UnitTransition a a a a a a
                      deriving (Functor, Foldable, Traversable, Show)
commandTransitions :: UnitTransition Command
commandTransitions = UnitTransition MoveW MoveE MoveSW MoveSE RotateCW RotateCCW
instance Applicative UnitTransition where
  pure a = UnitTransition a a a a a a
  UnitTransition a b c d e f <*> UnitTransition a' b' c' d' e' f' = UnitTransition (a a') (b b') (c c') (d d') (e e') (f f')
type instance Key UnitTransition = Command
instance Lookup UnitTransition where lookup = lookupDefault
instance Indexable UnitTransition where
  index (UnitTransition a b c d e f) MoveW = a
  index (UnitTransition a b c d e f) MoveE = b
  index (UnitTransition a b c d e f) MoveSW = c
  index (UnitTransition a b c d e f) MoveSE = d
  index (UnitTransition a b c d e f) RotateCW = e
  index (UnitTransition a b c d e f) RotateCCW = f
-- This data structure is infinite if the graph is cyclic
-- Transitions are O(1) !
data GraphEntry = GraphEntry
                  { _gePosition    :: Position
                  , _gePositionId  :: Int
                  , _geMembers     :: [(Int, Int)]
                  , _geUpdate      :: FillMap -> FillMap
                  , _geTransitions :: UnitTransition (Maybe GraphEntry)
                  }

data UnitData = UnitData
                { _unitGraph   :: Map Position GraphEntry
                , _unitInitial :: GraphEntry
                , _unitSize    :: Int
                }

type FillMap = Vector (VU.Vector Bool)

makeLenses ''GraphEntry
makeLenses ''UnitData

computeUnitData :: Int -> Int -> Unit -> UnitData
computeUnitData w h u = udata
  where pivotESE = u^.unitPivot.to toESE
        members :: Position -> [(Int, Int)]
        members (p, r) = u ^. unitMembers
                         <&> (toESE >>> (^-^ pivotESE) >>> rotESE r >>> (^+^ (pivotESE ^+^ p)) >>> fromESE)
        r2 = Set.fromList (members (V2 0 0, RSW)) == Set.fromList (u^.unitMembers)
        r3 = Set.fromList (members (V2 0 0, RW)) == Set.fromList (u^.unitMembers)
        normRot :: Rotation -> Rotation
        normRot = toEnum . (`mod` (6`div`(if r2 then 3 else 1)`div`(if r3 then 2 else 1))) . fromEnum
        initl = minimum . fmap fst $ u^.unitMembers
        initr = maximum . fmap fst $ u^.unitMembers
        init = (V2 ((w-initl-initr-1)`div`2) 0, RE)

        valid = getAll . foldMap (bifoldMap (\x -> All (x >= 0 && x < w)) (\y -> All (y >= 0 && y < h))) . members

        -- RecursiveDo : only one map traversal !
        go :: Position -> State (Int, Map Position GraphEntry) (Maybe GraphEntry)
        go p | valid p = mdo
                 a <- (_2.at p <<.= Just a)
                      >>= maybe (do
                                     ix <- _1 <<+= 1
                                     GraphEntry p ix (members p) (makeUpdate $ members p)
                                       <$> (UnitTransition
                                            <$> go (p&_1._x-~1) <*> go (p&_1._x+~1)
                                            <*> go (p&_1._x-~1&_1._y+~1) <*> go (p&_1._y+~1)
                                            <*> go (p&_2%~(rotateCW >>> normRot)) <*> go (p&_2%~(rotateCCW >>> normRot)))
                                ) pure
                 pure $ Just a
             | otherwise = pure Nothing

        udata = let (Just a, (_, g)) = runState (go init) (0, Map.empty)
                in UnitData g a (u^.unitMembers.to length)

makeUpdate :: [(Int, Int)] -> FillMap -> FillMap
makeUpdate l =
  let l' = l <&> swap & sort & groupBy ((==) `on` fst) <&> \(xs@((x,_):_)) -> (x, snd<$>xs)
  in \v -> v V.// (l' <&> \(x, ys) -> (x, (v `V.unsafeIndex` x) VU.// zip ys (repeat True)))

validEntry :: FillMap -> GraphEntry -> Bool
validEntry v e = all (\(x, y) -> not $ v `V.unsafeIndex` y `VU.unsafeIndex` x) (e^.geMembers)

data SimStep = SimStep
               { _simRandom    :: Int
               , _simFillMap   :: FillMap
               , _simPosition  :: GraphEntry
               }
makeLenses ''SimStep

simulateNextUnit :: Monad m => Int -> Int -> Vector UnitData -> StateT SimStep m ()
simulateNextUnit w h units = do
  n <- fmap ((.&. 0x7FFF) . flip shiftR 16 . fromIntegral)
       $ simRandom <<%= (.&. 0xFFFFFFFF) . (+ 12345) . (* 1103515245)
  traceShowM n
  simPosition .= units V.! (n `mod` V.length units) ^. unitInitial

simulateCommand :: Monad m => Int -> Int -> Vector UnitData -> Command -> StateT SimStep m Bool
simulateCommand w h u c = do
  use (simPosition.geTransitions) <&> (index ?? c)
    >>= \case
    Nothing -> do
      a <- use simPosition
      simFillMap %= snd . clearFulls . (a^.geUpdate)
      simulateNextUnit w h u
      pure True
    Just a  -> do
      v <- use simFillMap
      if validEntry v a
        then do
        simPosition .= a
        pure True
        else do
        a <- use simPosition
        simFillMap %= snd . clearFulls . (a^.geUpdate)
        simulateNextUnit w h u
        pure True


simulate :: Int -> FillMap -> Int -> Int -> Vector UnitData -> [Command] -> IO ()
simulate s v w h u cs = evalStateT
                    (let a (c:cs) = do
                           b <- simulateCommand w h u c
                           v <- use simFillMap
                           liftIO $ print b
                           liftIO $ print c
                           u <- use simPosition
                           liftIO $ print (u^.gePosition)
                           liftIO $ printMap (\i j -> (v & u^.geUpdate) V.! j VU.! i) w h
                           if b then a cs else liftIO $ print cs
                         a [] = pure ()
                     in do
                       simulateNextUnit w h u
                       v <- use simFillMap
                       u <- use simPosition
                       liftIO $ printMap (\i j -> (v & u^.geUpdate) V.! j VU.! i) w h
                       a cs
                    ) (SimStep s v undefined)


-- TODO : find a way to make this incremental
-- TODO : change this to a bfs -> ALT

-- BFS traversal
-- State :
-- _1 : explored node
-- _2 : bfs queue
-- _3 : how to reach this position
-- _4 : final result : how to reach this position and lock
findReachable_ :: AC -> OCommandCache -> FillMap -> IntMap (OState Char) ->
                  State (IntSet, Seq (GraphEntry, OState Char), IntMap (OState Char), [((Int, FillMap), OState Char)]) ()
findReachable_ ac cc v mp = do
  _2 %%= maybe (Nothing, Empty) (first Just) . uncons
    >>= \case
      Nothing -> pure ()
      Just (ge, o) -> do
        let cs = fromJust (lookup (ge^.gePositionId) mp)
        uex <- _1.contains (ge^.gePositionId) <<.= True
        _3 %= IntMap.insertWith (IntMap.unionWith maxEntry) (ge^.gePositionId) o
        when (not uex) $ do
          b <- forM ((,) <$> commandTransitions <*> ge ^. geTransitions) $ \(c, a) -> do
            ex <- use _1
            let d = do e <- maybe (Left False) Right a -- Left False : can be used to stop the current unit
                       when (IntSet.member (e^.gePositionId) ex) (Left True) -- Left True : we fail if we do this
                       when (not $ validEntry v e) (Left False)
                       pure e
            forM d $ \gd -> _2 %= (:> (gd, oacCache (cc [c]) cs))
          let c = (,) <$> commandTransitions <*> b
                  & toList & filter ((== Left False).snd) <&> fst
          when (not (null c)) $ do
            _4 %= ( (clearFulls (ge^.geUpdate $ v)
                    , oacCache (cc c) cs) :)
        findReachable_ ac cc v mp

findReachable :: AC -> OCommandCache -> FillMap -> OState Char -> UnitData -> [((Int, FillMap), OState Char)]
findReachable ac cc v s u = let (_, _, mp, r) = execState (findReachable_ ac cc v mp) (mempty, Seq.singleton (u^.unitInitial, s), mempty, mempty)
                            in r

clearFulls :: FillMap -> (Int, FillMap)
clearFulls v = let v' = V.filter (not . VU.foldr (&&) True) v
                   ls = V.length v - V.length v'
               in (ls, V.replicate ls (VU.replicate (VU.length (V.head v)) False) <> v')

data SolveStep = SolveStep
                 { _stepRandom    :: Int
                 , _stepRunning   :: Bool
                 , _stepFillMap   :: FillMap
                 , _stepScore     :: Int
                 , _stepLastLines :: Int
                 , _stepOState    :: OState Char

                 , _stepFillScore :: Ratio Integer
                 , _stepAcc       :: Int
                   -- ^ Fill Score : it is better to fill nonempty lines
                   -- ^              it is better to fill lines with high height (as we spawn from low heights)
                 }
makeLenses ''SolveStep

getFillScore :: Int -> Int -> FillMap -> Ratio Integer
getFillScore w h v = let w' = fromIntegral w :: Integer
                         h' = fromIntegral h :: Integer in
                     V.sum $ V.imap (\i v' -> let a = VU.foldl' (\x -> (+ x) . bool 0 1) 0 v'
                                              in (60*(a+1)*a) % (w'*w') -- Full line ~ 50pts = half the score from clearing a line
                                                 - 40 * (a * (fromIntegral $ let x = h - 1 - i
                                                                             in x^(2 :: Integer))) % (h'*h')
                                    ) v

getAcc :: Int -> Int -> FillMap -> Int
getAcc w h v = go 0 (V.toList v) (VU.replicate w True)
  where go i []     a = 0
        go i (v:vs) a = let a' = VU.zipWith (&&) (VU.map not v) a
                        in VU.foldl' (\x -> (+ x) . bool 0 1) 0 a'
                           + go (i+1) vs (VU.generate w $ if i`mod`2 == (0 :: Integer)
                                                          then \i -> a' VU.! i || (if i+1<w then a' VU.! (i+1) else False)
                                                          else \i -> a' VU.! i || (if i>0 then a' VU.! (i-1) else False)
                                         )

rankStep :: Int -> Int -> SolveStep -> Ratio Integer
rankStep w h s =
  (fromIntegral (s ^. stepScore)
   + (s ^. stepFillScore)
   + fromIntegral (s ^. stepAcc) % 1
   -- + (stateScore (bestOState (s^.stepOState))) % 2
   - if s ^. stepRunning then 0 else 1000 -- Losing is bad (but this measure is also bad)
  )

rankStep2 :: Int -> Int -> SolveStep -> Ratio Integer
rankStep2 w h s = fromIntegral (s ^. stepScore) + (stateScore (bestOState (s^.stepOState))) % 1

data SolveEnv = SolveEnv
                { _sWidth :: Int
                , _sHeight :: Int
                , _sUnits :: Vector UnitData
                , _sBranching :: Int
                , _sDepth :: Int

                , _sAC :: AC
                , _sCommandCache :: OCommandCache
                }
makeLenses ''SolveEnv

singleStep :: SolveStep -> Reader SolveEnv [SolveStep]
singleStep s
  | s ^. stepRunning = do
      let n = (.&. 0x7FFF) . flip shiftR 16 . fromIntegral $ s^.stepRandom
          nr = (.&. 0xFFFFFFFF) . (+ 12345) . (* 1103515245) $ s^.stepRandom
          v = s ^. stepFillMap
      w <- view sWidth
      h <- view sHeight
      us <- view sUnits
      branching <- view sBranching
      ac <- view sAC
      cc <- view sCommandCache
      let u = us V.! (n `mod` V.length us)
      if validEntry v (u^.unitInitial)
        then do
        let r = findReachable ac cc v (s^.stepOState) u
            ss = r <&> \((l, v'), cs) -> SolveStep nr True v'
                                         (s^.stepScore +
                                          let points = (u^.unitSize) + 50*(1+l)*l
                                              lsln = s^.stepLastLines
                                          in points + if lsln > 1 then ((lsln-1)*points+9)`div`10 else 0
                                         )
                                         l
                                         cs
                                         (getFillScore w h v')
                                         (getAcc w h v')
        pure $ ss
          -- & foldr (\a -> IntMap.insertWith (++) (a^.stepLastLines) [a]) IntMap.empty
          -- & IntMap.toList & reverse
          & sortBy (flip compare `on` rankStep w h) 
          -- & transpose & concat
          & take branching
        else pure [s & stepRunning .~ False]
  | otherwise = pure [s]

solveTree :: SolveStep -> Reader SolveEnv (Tree SolveStep)
solveTree s = Node s <$> (singleStep s >>= mapM solveTree)

pickOne :: Bool -> String -> Int -> Tree SolveStep -> ReaderT SolveEnv IO SolveStep
pickOne pm s 0 (Node a _)  = do
  w <- view sWidth
  h <- view sHeight
  when pm $ do
    liftIO $ printMap (\i j -> (a^.stepFillMap) V.! j VU.! i) w h
    liftIO $ print (a ^. stepScore + stateScore (bestOState (a ^. stepOState)))
  pure a
pickOne pm s i (Node a as) | not (a ^. stepRunning) = pure a
                        | otherwise = do
  w <- view sWidth
  h <- view sHeight
  if not pm
    then liftIO $ do
    putChar '\r'
    putStr $ s ++ show i ++ " " ++ show (a ^. stepScore) ++ "    "
    hFlush stdout
    else do
    liftIO $ printMap (\i j -> (a^.stepFillMap) V.! j VU.! i) w h
    liftIO $ putStrLn (s ++ " " ++ show i ++ " " ++ show (a ^. stepScore + stateScore (bestOState (a ^. stepOState))))
    liftIO $ putStrLn ""
  depth <- view sDepth
  if i <= depth
    then pickOne pm s (i-1) $ as & maximumBy (compare `on` (maximum . fmap (rankStep2 w h) . (!! (min i depth - 1)) . levels))
    else pickOne pm s (i-1) $ as & maximumBy (compare `on` (maximum . fmap (rankStep w h) . (!! (min i depth - 1)) . levels))
