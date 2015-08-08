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
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Diagrams.Prelude as D
import qualified Graphics.Rendering.Chart.Easy as C

import Debug.Trace

import Game

data Rotation = RE | RSE | RSW | RW | RNW | RNE
              deriving (Eq, Ord, Enum, Bounded, Show)

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

toBaseESE :: (Int, Int) -> (Int, Int)
toBaseESE (x, y) = (x - y `div` 2, y)
fromBaseESE :: (Int, Int) -> (Int, Int)
fromBaseESE (x, y) = (x + y `div` 2, y)

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

encodeInt i | i >= 0 = 2*i
            | otherwise = 2*(-i)+1
decodeInt i | i `mod` 2 == 0 = i `div` 2
            | otherwise  = - i`div`2

encodePosition :: Int -> Int -> Position -> Int
encodePosition w h ((x, y), r) = fromEnum r+6*(encodeInt y+2*h*encodeInt x)

decodePosition :: Int -> Int -> Int -> Position
decodePosition w h i = let (xy, r) = i `divMod` 6
                           (x, y) = xy `divMod` (2*h)
                       in ((decodeInt x, decodeInt y), toEnum r)


members :: Unit -> Position -> [(Int, Int)]
members u (p, r) = let pivotESE = u^.unitPivot.to toBaseESE
                   in u^.unitMembers&fmap (toBaseESE >>> subtract pivotESE >>> rotESE r >>> (+ pivotESE) >>> (+ toBaseESE p) >>> fromBaseESE)


computeUnitData :: Int -> Int -> Unit -> (Position, Gr (Maybe Command) Command)
computeUnitData w h u = (init, snd (execState (go init) (mempty, mkGraph [] [])))
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

    -- RecursiveDo !
    go :: Position -> State (Set Position, Gr (Maybe Command) Command) Bool
    go (p@(x,y),r) = do
      let e = encodePosition w h (p, r)
      b <- _1 . contains (p, r) <<.= True
      let v = valid (members u (p, r))
      when (v && not b) $ mdo
        _2 %= insNode (e, v)
        let psw = if y`mod`2 == 0 then (x-1, y+1) else (x, y+1)
        let pse = if y`mod`2 == 0 then (x, y+1) else (x+1, y+1)
        let np = [ ((psw, r), MoveSW)
                 , ((pse, r), MoveSE)
                 , (((x+1, y), r), MoveE)
                 , (((x-1, y), r), MoveW)
                 -- , ((p, normRot (rotateCW r)), RotateCW)
                 -- , ((p, normRot (rotateCCW r)), RotateCCW)
                 ]
        np' <- forM np $ \(b, a) -> do
          c <- go b
          when c (_2 %= insEdge (e, encodePosition w h b, a))
          pure (c, a)
        let v = foldr1 (<|>) (np' <&> \(b, a) -> do guard (not b); Just a)
        pure ()
      pure v

data SolverState = SolverState
                   { _stateRunning :: Bool
                   , _stateRandom  :: Integer
                   , _stateWidth   :: Int
                   , _stateHeight  :: Int
                   , _stateUnits   :: Vector (Unit, (Position, Gr (Maybe Command) Command))
                   , _stateGrid    :: Vector (Vector Bool)
                   }
makeLenses ''SolverState
type Solver a = StateT SolverState IO a

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

clearFulls :: Vector (Vector Bool) -> Vector (Vector Bool)
clearFulls v = let v' = V.filter (not . getAll . foldMap All) v
               in V.replicate (V.length v - V.length v') (V.replicate (V.length (V.head v)) False) <> v'

solveOne :: Solver [Command]
solveOne = do
  n <- fmap ((.&. 0x7FFF) . flip shiftR 16 . fromIntegral) $ stateRandom <<%= (`mod` (2^32)) . (+ 12345) . (* 1103515245)
  un <- use stateUnits
  v <- use stateGrid
  let (u, (init, ugr)) = un V.! (n `mod` V.length un)
  let okpos p = all (\(i, j) -> not $ v V.! j V.! i) $ members u p
  r <- stateRunning <%= (&& okpos init)
  w <- use stateWidth
  h <- use stateHeight
  if r then do
    -- liftIO $ clearScreen
    -- liftIO $ setCursorPosition 0 0
    liftIO $ printMap (\i j -> v V.! j V.! i) w h
    liftIO $ putStrLn ""
    -- liftIO $ printMap (\i j -> (i, j) `elem` members u init) w h
    -- liftIO $ putStrLn ""
    liftIO $ print init
    let endFrom i = snd (head (filter (not . okpos . decodePosition w h . fst) (lsuc' (context ugr i))))
        treePaths b (Node (i, (a, l)) f) = (i, fromMaybe (endFrom i) a:l++b) : concat (treePaths (l++b) <$> f)
    let ugr' = ugr
              & nfilter (okpos . decodePosition w h)
    let rgr = ugr'
              & ldffWith (fmap (second (:[])) . lsuc') [(encodePosition w h init, [])]
              & concat . fmap (treePaths [])
              & sortBy (compare `on` (length.snd))
              & reverse
    if null rgr
      then do
      pure []
      else do
      let (decodePosition w h -> ((x, y), r), c) = head rgr
          dt = members u ((x, y), r) & fmap (\(x, y) -> (y, x)) & sort & groupBy ((==) `on` fst) & fmap (\xs@((x,_):_) -> (x, snd <$> xs))
      liftIO $ print dt
      liftIO $ print (reverse c)
      stateGrid %= (V.// (dt <&> \(x, y) -> (x, v V.! x V.// zip y (repeat True))))
      stateGrid %= clearFulls
      pure (reverse c)
    else do
    traceShowM r
    pure []
