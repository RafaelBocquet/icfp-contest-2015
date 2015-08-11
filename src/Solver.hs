module Solver where

import Prelude ()
import MyPrelude

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.DList as DL
import System.IO.Unsafe
import Debug.Trace
import Unsafe.Coerce
import Control.Monad.ST
import System.Random

import Data.Tuple
import Linear hiding (transpose)

import Game
import Optimizer

import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ

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
                  , _geMemberId    :: Int
                  , _geUpdate      :: FillMap -> FillMap
                  , _gePivot       :: (Int, Int)
                  , _geTransitions :: UnitTransition (Maybe GraphEntry)
                  }

data UnitData = UnitData
                { _unitGraph     :: Map Position GraphEntry
                , _unitInitial   :: GraphEntry
                , _unitSize      :: Int
                , _unitPosCount  :: Int
                , _unitMemberMap :: Vector (FillMap -> FillMap)
                }

type FillMap = Vector Integer

makeLenses ''GraphEntry
makeLenses ''UnitData

computeUnitData :: Int -> Int -> Unit -> UnitData
computeUnitData w h u = udata
  where pivotESE = u^.unitPivot.to toESE
        toPosition (p, r) = toESE >>> (^-^ pivotESE) >>> rotESE r >>> (^+^ (pivotESE ^+^ p)) >>> fromESE
        members :: Position -> [(Int, Int)]
        members p = u ^. unitMembers <&> toPosition p
        pivot p = u ^. unitPivot & toPosition p
        r2 = Set.fromList (members (V2 0 0, RSW)) == Set.fromList (u^.unitMembers)
        r3 = Set.fromList (members (V2 0 0, RW)) == Set.fromList (u^.unitMembers)
        normRot :: Rotation -> Rotation
        normRot = toEnum . (`mod` (6`div`(if r2 then 3 else 1)`div`(if r3 then 2 else 1))) . fromEnum
        initl = minimum . fmap fst $ u^.unitMembers
        initr = maximum . fmap fst $ u^.unitMembers
        init = (V2 ((w-initl-initr-1)`div`2) 0, RE)

        memberSet  = Set.fromList (udata&_unitGraph&Map.elems<&>_geMembers<&>sort)
        memberList = Set.toList memberSet
        memberMap  = Map.fromList (zip memberList [0..])

        valid = getAll . foldMap (bifoldMap (\x -> All (x >= 0 && x < w)) (\y -> All (y >= 0 && y < h))) . members

        -- RecursiveDo : only one map traversal !
        go :: Position -> State (Int, Map Position GraphEntry) (Maybe GraphEntry)
        go p | valid p = mdo
                 a <- (_2.at p <<.= Just a)
                      >>= maybe (do idm <- _1 <<+= 1
                                    GraphEntry p idm
                                      (members p) (fromJust (lookup (sort (members p)) memberMap))
                                      (makeUpdate $ members p)
                                      (pivot p)
                                      <$> (UnitTransition
                                           <$> go (p&_1._x-~1) <*> go (p&_1._x+~1)
                                           <*> go (p&_1._x-~1&_1._y+~1) <*> go (p&_1._y+~1)
                                           <*> go (p&_2%~(rotateCW >>> normRot)) <*> go (p&_2%~(rotateCCW >>> normRot)))
                                ) pure
                 pure $ Just a
             | otherwise = pure Nothing

        udata = let (Just a, (mx, g)) = runState (go init) (0, Map.empty)
                in UnitData g a (u^.unitMembers.to length) (mx+1) (V.fromList (makeUpdate <$> memberList))

makeUpdate :: [(Int, Int)] -> FillMap -> FillMap
makeUpdate l =
  let l' = l <&> swap & sort & groupBy ((==) `on` fst) <&> (\(xs@((x,_):_)) -> (x, snd<$>xs))
           <&> second (foldl' setBit 0)
  in \v -> v V.// (l' <&> \(x, ys) -> (x, (v `V.unsafeIndex` x) .|. ys))

validEntry :: FillMap -> GraphEntry -> Bool
validEntry v e = all (\(x, y) -> not $ v `V.unsafeIndex` y `testBit` x) (e^.geMembers)

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
  simPosition .= units V.! (n `mod` V.length units) ^. unitInitial

simulateCommand :: Monad m => Int -> Int -> Vector UnitData -> Command -> StateT SimStep m Bool
simulateCommand w h u c = do
  use (simPosition.geTransitions) <&> (index ?? c)
    >>= \case
    Nothing -> do
      a <- use simPosition
      simFillMap %= snd . clearFulls w . (a^.geUpdate)
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
        simFillMap %= snd . clearFulls w . (a^.geUpdate)
        simulateNextUnit w h u
        pure True


simulate :: Int -> FillMap -> Int -> Int -> Vector UnitData -> [Command] -> IO ()
simulate s v w h u cs = evalStateT
                    (let a (c:cs) = do
                           b <- simulateCommand w h u c
                           v <- use simFillMap
                           liftIO $ hPrint stdout b
                           liftIO $ hPrint stdout c
                           u <- use simPosition
                           liftIO $ hPrint stdout (u^.gePosition)
                           liftIO $ printMap (\i j -> (v & u^.geUpdate) V.! j `testBit` i) w h
                           if b then a cs else liftIO $ hPrint stdout cs
                         a [] = pure ()
                     in do
                       simulateNextUnit w h u
                       v <- use simFillMap
                       u <- use simPosition
                       liftIO $ printMap (\i j -> (v & u^.geUpdate) V.! j `testBit` i) w h
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
findReachable_ :: AC -> OCommandCache -> FillMap -> Vector (OState Char) ->
                  StateT (STVector s Bool, MinPQueue Int (GraphEntry, OState Char), STVector s (OState Char), [(Int, OState Char)]) (ST s) ()
findReachable_ ac cc v mp = do
  _2 %%= maybe (Nothing, undefined) (first Just) . PQ.minView
    >>= \case
      Nothing -> pure ()
      Just (ge, o) -> do
        let cs = mp V.! (ge^.gePositionId)
        v1 <- use _1
        uex <- VM.read v1 (ge^.gePositionId)
        VM.write v1 (ge^.gePositionId) True
        v3 <- use _3
        VM.read v3 (ge^.gePositionId) <&> maxOState o >>= VM.write v3 (ge^.gePositionId)
        when (not uex) $ do
          b <- forM ((,) <$> commandTransitions <*> ge ^. geTransitions) $ \(c, a) -> do
            d <- case a of
              Nothing -> pure (Left False)
              Just e -> do
                xxxxx <- VM.read v1 (e^.gePositionId)
                pure $ if xxxxx
                       then Left True
                       else if validEntry v e then Right e else Left False
            forM d $ \gd -> _2 %= (PQ.insert (ge^.gePivot._2) (gd, oacCache (cc [c]) cs))
          let c = (,) <$> commandTransitions <*> b
                  & toList & filter ((== Left False).snd) <&> fst
          when (not (null c)) $ do
            _4 %= ( (ge^.geMemberId
                    , oacCache (cc c) cs) :)
        findReachable_ ac cc v mp

findReachable :: AC -> OCommandCache -> FillMap -> OState Char -> UnitData -> [(Int, OState Char)]
findReachable ac cc v s u =
  let (mp, p) = runST $ do
        xx <- VM.replicate (u^.unitPosCount) False
        yy <- VM.replicate (u^.unitPosCount) mempty
        (a,b,c,d) <- execStateT (findReachable_ ac cc v mp) (xx, PQ.singleton (u^.unitInitial.gePivot._2) (u^.unitInitial, s), yy, [])
        e <- V.freeze c
        pure (e, d)
  in p

clearFulls :: Int -> FillMap -> (Int, FillMap)
clearFulls w v = let v' = V.filter ((/= w) . popCount) v
                     ls = V.length v - V.length v'
                 in (ls, V.replicate ls 0 <> v')

data SolveStep = SolveStep
                 { _stepRandom    :: Int
                 , _stepRunning   :: Bool
                 , _stepFillMap   :: FillMap
                 , _stepScore     :: Int
                 , _stepOScore    :: Int
                 , _stepLastLines :: Int
                 , _stepOState    :: OState Char
                 }
makeLenses ''SolveStep

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

rankStep :: Int -> SolveStep -> [(String, Int)]
rankStep w a = do
  let v = a^.stepFillMap
      h = V.length v
  let ysum   = fromIntegral $ V.sum (V.imap (\i v' -> (h - i) * popCount v') v)
      xholes = V.sum (V.map (\v' -> foldl' (\a x -> a + bool 0 1 (testBit v' x /= testBit v' (x+1))) 0 [0..w-2]) v)
      seholes = sum $ [-(h-1)`div`2..w-1]
                <&> \i -> foldl' (\a j -> a + bool 0 1 (testBit (v V.! j) (i+j`div`2) /= testBit (v V.! (j+1)) (i+(j+1)`div`2)))
                          0 [max 0 (-2*i)..min (h-1) (2*(w-1-i)+1)-1]
      swholes = sum $ [0..w-1+h`div`2]
                <&> \i -> foldl' (\a j -> a + bool 0 1 (testBit (v V.! j) (i-j`div`2) /= testBit (v V.! (j+1)) (i-(j+1)`div`2)))
                          0 [max 0 (2*(i-w+1)-1)..min (h-1) (2*i)-1]
  id $ [ ("YSUM", - 3 * ysum)
       , ("XHOLES", - 3 * xholes)
       , ("SEHOLES", - seholes)
       , ("SWHOLES", - swholes)
       , ("LINES", fromIntegral $ 3 * (a^.stepLastLines) ^ 2)
       , ("RUNNING", if a ^. stepRunning then 0 else -1000)
         -- , ("SCORE", fromIntegral $ a^.stepScore)
         -- , ("OSCORE", fromIntegral $ a^.stepOScore)
       ]

stepFullScore :: SolveStep -> Int
stepFullScore s = if s^.stepRunning
                  then s^.stepScore + (s^.stepOScore)
                  else (s^.stepScore + (s^.stepOScore))`div`2

sumRankStep :: Int -> SolveStep -> Int
sumRankStep w = sum . fmap snd . rankStep w

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
                & IntMap.fromListWith maxOState & IntMap.toList
                <&> first (\x -> clearFulls w (((u^.unitMemberMap) V.! x) v))
            ss = r <&> \((l, v'), cs) -> SolveStep nr True v'
                                         (s^.stepScore +
                                          let points = (u^.unitSize) + 50*(1+l)*l
                                              lsln = s^.stepLastLines
                                          in points + if lsln > 1 then ((lsln-1)*points+9)`div`10 else 0)
                                         (stateScore (bestOState cs))
                                         l
                                         cs
        pure $ ss
          & sortBy (flip compare `on` sumRankStep w)
          & take branching
        else pure [s & stepRunning .~ False]
  | otherwise = pure [s]

solveTree :: SolveStep -> Reader SolveEnv (Tree SolveStep)
solveTree s = Node s <$> (singleStep s >>= mapM solveTree)

pickOne :: Bool -> Bool -> String -> Int -> Tree SolveStep -> ReaderT SolveEnv IO SolveStep
pickOne pm int s i (Node a _) | i == 0 || not (a ^. stepRunning) = do
  w <- view sWidth
  h <- view sHeight
  when pm $ do
    liftIO $ printMap (\i j -> (a^.stepFillMap) V.! j `testBit` i) w h
    liftIO $ hPrint stdout (a ^. stepScore + stateScore (bestOState (a ^. stepOState)))
  pure a
pickOne pm int s i (Node a as) = (a^.stepOScore) `seq` do
  w <- view sWidth
  h <- view sHeight
  liftIO $ if not pm
           then do
             hPutChar stdout '\r'
             hPutStr stdout $ s ++ show i ++ " " ++ show (a ^. stepScore) ++ "    "
             hFlush stdout
           else do
             printMap (\i j -> (a^.stepFillMap) V.! j `testBit` i) w h
             hPrint stdout (rankStep w a)
             hPutStrLn stdout (s ++ " " ++ show i ++ " " ++ show (a ^. stepScore + stateScore (bestOState (a ^. stepOState))))
             hPutStrLn stdout " === children === "
             when int $ do
               forM_ as $ \(Node a _) -> do
                 printMap (\i j -> (a^.stepFillMap) V.! j `testBit` i) w h
                 hPrint stdout (rankStep w a)
                 hPutStrLn stdout (s ++ " " ++ show i ++ " " ++ show (a ^. stepScore + stateScore (bestOState (a ^. stepOState))))
                 hPutStrLn stdout ""
               void getChar
  depth <- view sDepth
  if i <= depth
    then pickOne pm int s (i-1) $ as & maximumBy (compare `on` (maximum . fmap stepFullScore . (!! (min i depth - 1)) . levels))
    else pickOne pm int s (i-1) $ as & maximumBy (compare `on` (maximum . fmap (sumRankStep w) . (!! (min i depth - 1)) . levels))
