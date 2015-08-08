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

import Game

data Rotation = RE | RSE | RSW | RW | RNW | RNE
              deriving (Eq, Ord, Enum, Bounded, Show)

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

{-# INLINE toBaseESE #-}
{-# INLINE fromBaseESE #-}
toBaseESE :: (Int, Int) -> (Int, Int)
toBaseESE (x, y) = (x - y `div` 2, y)
fromBaseESE :: (Int, Int) -> (Int, Int)
fromBaseESE (x, y) = (x + y `div` 2, y)

{-# INLINE rotESE #-}
rotESE :: Rotation -> (Int, Int) -> (Int, Int)
rotESE RE (x, y) = (x, y)
rotESE RSE (x, y) = (-y, x+y)
rotESE RSW (x, y) = (-x-y, x)
rotESE RW (x, y) = (-x, -y)
rotESE RNW (x, y) = (y, -x-y)
rotESE RNE (x, y) = (x+y, -x)

instance Num (Int, Int) where
  (a, b) + (x, y) = (a+x, b+y)
  (a, b) - (x, y) = (a-x, b-y)
  negate (x, y)   = (-x, -y)

type Position = ((Int, Int), Rotation)

{-# INLINE encodeInt #-}
{-# INLINE decodeInt #-}
encodeInt, decodeInt :: Int -> Int
encodeInt i | i >= 0 = 2*i
            | otherwise = 2*(-i)+1
decodeInt i | i `mod` 2 == 0 = i `div` 2
            | otherwise  = - i`div`2

{-# INLINE encodePosition #-}
encodePosition :: Int -> Int -> Position -> Int
encodePosition w h ((x, y), r) = fromEnum r+6*(encodeInt y+2*(w+h+2)*encodeInt x)

{-# INLINE decodePosition #-}
decodePosition :: Int -> Int -> Int -> Position
decodePosition w h i = let (xy, r) = i `divMod` 6
                           (x, y) = xy `divMod` (2*(w+h+2))
                       in ((decodeInt x, decodeInt y), toEnum r)


members :: Unit -> Position -> [(Int, Int)]
members u (p, r) = let pivotESE = u^.unitPivot.to toBaseESE
                   in u^.unitMembers&fmap (toBaseESE >>> subtract pivotESE >>> rotESE r >>> (+ pivotESE) >>> (+ toBaseESE p) >>> fromBaseESE)


computeUnitData :: Int -> Int -> Unit -> (Position, Gr (Maybe Command) Command, IntMap [(Int, Int)])
computeUnitData w h u = let ss = execState (go init) (mempty, mkGraph [] [], IntMap.empty) in (init, ss^._2, ss^._3)
  where
    r2 = Set.fromList (members u ((0,0), RSW)) == Set.fromList (u^.unitMembers)
    r3 = Set.fromList (members u ((0,0), RW)) == Set.fromList (u^.unitMembers)
    normRot :: Rotation -> Rotation
    normRot = toEnum . (`mod` (6`div`(if r2 then 3 else 1)`div`(if r3 then 2 else 1))) . fromEnum
    initl = minimum . fmap fst $ u^.unitMembers
    initr = maximum . fmap fst $ u^.unitMembers
    init = (((w-3*initl-initr-1)`div`2, 0), RE)
    allR = [minBound..maxBound] :: [Rotation]
    valid :: [(Int, Int)] -> Bool
    valid = getAll . foldMap (bifoldMap (\x -> All (x >= 0 && x < w)) (\y -> All (y >= 0 && y < h)))
    getM :: Position -> State (Set Position, Gr (Maybe Command) Command, IntMap [(Int, Int)]) [(Int, Int)]
    getM p = fmap fromJust $ _3.at (encodePosition w h p) <%= Just . maybe (members u p) id

    -- RecursiveDo !
    go :: Position -> State (Set Position, Gr (Maybe Command) Command, IntMap [(Int, Int)]) Bool
    go (p@(x,y),r) = do
      let e = encodePosition w h (p, r)
      b <- _1 . contains (p, r) <<.= True
      v <- valid <$> getM (p, r)
      when (v && not b) $ mdo
        _2 %= insNode (e, v)
        let psw = if y`mod`2 == 0 then (x-1, y+1) else (x, y+1)
        let pse = if y`mod`2 == 0 then (x, y+1) else (x+1, y+1)
        let np = [ ((psw, r), MoveSW)
                 , ((pse, r), MoveSE)
                 , (((x+1, y), r), MoveE)
                 , (((x-1, y), r), MoveW)
                 , ((p, normRot (rotateCW r)), RotateCW)
                 , ((p, normRot (rotateCCW r)), RotateCCW)
                 ]
        np' <- forM np $ \(b, a) -> do
          c <- go b
          when c (_2 %= insEdge (e, encodePosition w h b, a))
          pure (c, a)
        let v = foldr1 (<|>) (np' <&> \(b, a) -> do guard (not b); Just a)
        pure ()
      pure v

data SolverState = SolverState
                   { _stateRunning  :: Bool
                   , _stateRandom   :: Integer
                   , _stateWidth    :: Int
                   , _stateHeight   :: Int
                   , _stateUnits    :: Vector (Unit, (Position, Gr (Maybe Command) Command, IntMap [(Int, Int)]))
                   , _stateGrid     :: Vector (VU.Vector Bool)
                   , _stateCommands :: [Command]
                   , _stateLines    :: Int
                   , _stateScore    :: Int
                   , _stateTONAME   :: Int
                   }
makeLenses ''SolverState

ldfWith :: Graph gr
           => CFun a b [(Node, c)]
           -> [(Node, c)]
           -> gr a b
           -> ([Tree (Node, (a, c))],gr a b)
ldfWith _ []     g             = ([],g)
ldfWith _ _      g | isEmpty g = ([],g)
ldfWith d ((v, m):vs) g = case match v g of
  (Nothing,g1) -> ldfWith d vs g1
  (Just c@(_,_,e,_),g1)  -> (Node (v, (e, m)) ts:ts',g3)
    where (ts, g2) = ldfWith d (d c) g1
          (ts', g3) = ldfWith d vs g2
ldffWith :: Graph gr => CFun a b [(Node, c)] → [(Node, c)] → gr a b → [Tree (Node, (a, c))]
ldffWith a b c = fst (ldfWith a b c)

-- TODO : need to associate "impossible / stop" moves to nodes
-- TODO impossible move -> more choice to create phrases of power

clearFulls :: Vector (VU.Vector Bool) -> (Int, Vector (VU.Vector Bool))
clearFulls v = let v' = V.filter (not . VU.foldr (&&) True) v
                   cleared = V.length v - V.length v'
               in (cleared, V.replicate cleared (VU.replicate (VU.length (V.head v)) False) <> v')

branching, depth :: Int
branching = 2
depth = 2

plop n = n*(n+1)`div`2

solveOne :: SolverState -> [SolverState]
solveOne s = do
  let n = (.&. 0x7FFF) . flip shiftR 16 . fromIntegral $ s^.stateRandom
      v = s^.stateGrid
      (u, (init, ugr, umems)) = (s^.stateUnits) V.! (n `mod` V.length (s^.stateUnits))
      w = s^.stateWidth
      h = s^.stateHeight
      okpos p = all (\(i, j) -> not $ v V.! j VU.! i) $ fromJust (lookup (encodePosition w h p) umems)
      r = s^.stateRunning && okpos init
      s' = s & stateRunning .~ r & stateRandom %~ (`mod` (2^32)) . (+ 12345) . (* 1103515245)
  if r
    then do
    let endFrom i = case filter (not . okpos . decodePosition w h . fst) (lsuc' (context ugr i)) of
          [] -> Nothing; (_,a):_ -> Just a
        treePaths :: DList Command -> Tree (Node, (Maybe Command, DList Command)) -> DList (Node, DList Command)
        treePaths b (Node (i, (a, l)) f) = maybe id (\x -> ((i, x`DL.cons`l<>b) `DL.cons`)) (a <|> endFrom i) (mconcat (treePaths (l<>b) <$> f))
        r1 = ugr & nfilter (okpos . decodePosition w h)
        r2 = r1 & ldffWith (fmap (second (DL.cons ?? DL.empty)) . lsuc') [(encodePosition w h init, DL.empty)]
        r3 = r2 <&> treePaths DL.empty & mconcat & DL.toList
        r4 = r3 <&> (\(p@(decodePosition w h -> ((x, y), r)), DL.toList -> c) -> do
                         let dt = fromJust (lookup p umems)
                                  <&> (\(x, y) -> (y, x))
                                  & sort & groupBy ((==) `on` fst)
                                  <&> (\xs@((x,_):_) -> (x, snd <$> xs))
                             v' = v V.// (dt <&> \(x, y) -> (x, v V.! x VU.// zip y (repeat True)))
                             (ls, v'') = clearFulls v'
                             points  = length (u^.unitMembers) + 50*(ls+1)*ls
                             lsln    = s^.stateLines
                             tpoints = points + if lsln > 1 then ((lsln-1)*points+9)`div`10 else 0
                             toname  = V.sum (plop . VU.foldr (bool id (+1)) 0 <$> v'')
                         (ls, reverse c, v'', tpoints, toname)
                    )
        us (ls, c, v', pts, tn) = s'
                                  & stateTONAME .~ tn
                                  & stateCommands %~ (++ c)
                                  & stateGrid .~ v'
                                  & stateLines .~ ls
                                  & stateScore +~ pts
    (r1 `deepseq`) $ (r2 `deepseq`) $ r4 & sortBy (compare `on` (\(_,cmd,_,pts,tn) -> (-pts, -tn, -length cmd))) & take branching & fmap us
    else [s']

stateTree :: SolverState -> (Tree SolverState)
stateTree s = Node s (stateTree <$> solveOne s)

rankState :: SolverState -> Ratio Integer
rankState s =
  let w = fromIntegral $ s ^. stateWidth in
  fromIntegral (s ^. stateScore)
  + (50 % (w*w)) * fromIntegral (s ^. stateTONAME)

pickOne :: Int -> Tree SolverState -> IO SolverState
pickOne 0 (Node a _)  = do
  liftIO $ printMap (\i j -> (a^.stateGrid) V.! j VU.! i) (a^.stateWidth) (a^.stateHeight)
  print (a ^. stateScore)
  pure a
pickOne i (Node a as) = do
  liftIO $ printMap (\i j -> (a^.stateGrid) V.! j VU.! i) (a^.stateWidth) (a^.stateHeight)
  liftIO $ print (a ^. stateScore)
  liftIO $ putStrLn ""
  -- pickOne (i-1) $ as & head
  pickOne (i-1) $ as & maximumBy (compare `on` (maximum . fmap rankState . (!! depth) . levels))
