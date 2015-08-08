module Solver where

import Prelude ()
import MyPrelude hiding (head, fromJust)
import Debug.Trace

import Data.List.Located (head)
import Data.Maybe.Located (fromJust)

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

import Data.Tuple
import Linear

import Game

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
-- This data structure is infinite if the graph is cyclic
-- Transitions are O(1) !
data GraphEntry = GraphEntry
                  { _gePosition    :: Position
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
        init = (V2 ((w-3*initl-initr-1)`div`2) 0, RE)

        valid = getAll . foldMap (bifoldMap (\x -> All (x >= 0 && x < w)) (\y -> All (y >= 0 && y < h))) . members

        -- RecursiveDo : only one map traversal !
        go :: Position -> State (Map Position GraphEntry) (Maybe GraphEntry)
        go p | valid p = mdo
                 a <- (at p <<.= Just a)
                      >>= maybe (GraphEntry p (members p) (makeUpdate $ members p)
                                 <$> (UnitTransition
                                      <$> go (p&_1._x-~1) <*> go (p&_1._x+~1)
                                      <*> go (p&_1._x-~1&_1._y+~1) <*> go (p&_1._y+~1)
                                      <*> go (p&_2%~(rotateCW >>> normRot)) <*> go (p&_2%~(rotateCCW >>> normRot)))
                                ) pure
                 pure $ Just a
             | otherwise = pure Nothing

        udata = let (Just a, g) = runState (go init) Map.empty
                in UnitData g a (u^.unitMembers.to length)

makeUpdate :: [(Int, Int)] -> FillMap -> FillMap
makeUpdate l =
  let l' = l <&> swap & sort & groupBy ((==) `on` fst) <&> \(xs@((x,_):_)) -> (x, snd<$>xs)
  in \v -> v V.// (l' <&> \(x, ys) -> (x, (v V.! x) VU.// zip ys (repeat True)))

validEntry :: FillMap -> GraphEntry -> Bool
validEntry v e = all (\(x, y) -> not $ v V.! y VU.! x) (e^.geMembers)

findReachable_ :: FillMap -> DList Command -> GraphEntry -> State IntSet (DList ((Int, FillMap), DList Command))
findReachable_ v cs ge = do
  contains (ge ^. gePosition.to hash) .= True
  b <- forM ((,) <$> commandTransitions <*> ge ^. geTransitions) $ \(c, a) -> do
    ex <- get
    let d = do e <- maybe (Left False) Right a -- Left False : can be used to stop the current unit
               when (IntSet.member (hash $ e^.gePosition) ex) (Left True) -- Left True : we fail if we do this
               when (not $ validEntry v e) (Left False)
               pure e
    forM d $ findReachable_ v (DL.snoc cs c)
  let c = (,) <$> commandTransitions <*> b & toList & filter ((== Left False).snd) & fmap fst
  pure $ maybe id (DL.cons . (clearFulls (ge^.geUpdate $ v),) . DL.snoc cs) (c^?_head) $ foldMap fold b

findReachable :: FillMap -> UnitData -> DList ((Int, FillMap), DList Command)
findReachable v u = evalState (findReachable_ v DL.empty (u^.unitInitial)) IntSet.empty

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
                 , _stepCommands  :: DList Command

                 , _stepFillScore :: Int -- ^ Fill Score : it is better to fill nonempty lines
                 , _stepLowScore :: Int -- ^ Low Score : it is better to fill lines with high height (as we spawn from low heights)
                 }
makeLenses ''SolveStep

getFillScore :: FillMap -> Int
getFillScore = V.sum . V.map ((\x -> ((x+1)*x)`div`2) . VU.foldr (bool id (+1)) 0)

getLowScore :: FillMap -> Int
getLowScore v = V.sum . V.imap (\i -> VU.foldr (bool id (+ (V.length v-1-i))) 0) $ v

branching, depth :: Int
branching = 2
depth     = 3

rankStep :: Int -> Int -> SolveStep -> Ratio Integer
rankStep w h s =
  let w' = fromIntegral w in
  let h' = fromIntegral h in
  fromIntegral (s ^. stepScore)
  + (50 % (w'*w')) * fromIntegral (s ^. stepFillScore) -- Full line ~ 50pts = half the score from clearing a line
  - (1 % h') * fromIntegral (s ^. stepLowScore) 
  - if s ^. stepRunning then 0 else 400 -- Losing is bad (but this measure is also bad)


singleStep :: SolveStep -> Reader (Int, Int, Vector UnitData) [SolveStep]
singleStep s
  | s ^. stepRunning = do
      let n = (.&. 0x7FFF) . flip shiftR 16 . fromIntegral $ s^.stepRandom
          nr = (.&. 0xFFFFFFFF) . (+ 12345) . (* 1103515245) $ s^.stepRandom
          v = s ^. stepFillMap
      (w, h, us) <- ask
      let u = us V.! (n `mod` V.length us)
      if validEntry v (u^.unitInitial)
        then do
        let r = DL.toList (findReachable v u)
            ss = r <&> \((l, v'), cs) -> SolveStep nr True v'
                                         (s^.stepScore +
                                          let points = (u^.unitSize) + 50*(1+l)*l
                                              lsln = s^.stepLastLines
                                          in points + if lsln > 1 then ((lsln-1)*points+9)`div`10 else 0
                                         )
                                         l
                                         (s^.stepCommands <> cs)
                                         (getFillScore v')
                                         (getLowScore v')
        pure $ take branching (sortBy (flip compare `on` rankStep w h) ss)
        else pure [s & stepRunning .~ False]
  | otherwise = pure [s]

solveTree :: SolveStep -> Reader (Int, Int, Vector UnitData) (Tree SolveStep)
solveTree s = Node s <$> (singleStep s >>= mapM solveTree)

pickOne :: String -> Int -> Int -> Int -> Tree SolveStep -> IO SolveStep
pickOne s w h 0 (Node a _)  = do
  -- liftIO $ printMap (\i j -> (a^.stepFillMap) V.! j VU.! i) w h
  -- print (a ^. stepScore)
  pure a
pickOne s w h i (Node a as) = do
  liftIO $ do
    putChar '\r'
    putStr $ s ++ show i ++ "    "
    hFlush stdout
  -- liftIO $ printMap (\i j -> (a^.stepFillMap) V.! j VU.! i) w h
  -- liftIO $ print (a ^. stepScore)
  -- liftIO $ putStrLn ""
  pickOne s w h (i-1) $ as & maximumBy (compare `on` (maximum . fmap (rankStep w h) . (!! (min i depth - 1)) . levels))
