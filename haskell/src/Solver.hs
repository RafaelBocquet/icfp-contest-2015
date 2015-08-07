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
rotESE RSW (x, y) = (-x-y, -x)
rotESE RW (x, y) = (-x, -y)
rotESE RNW (x, y) = (y, -x-y)
rotESE RNE (x, y) = (x+y, -x)

instance Num (Int, Int) where
  (a, b) + (x, y) = (a+x, b+y)
  (a, b) - (x, y) = (a-x, b-y)
  negate (x, y)   = (-x, -y)

type Position = ((Int, Int), Rotation)

encodePosition :: Int -> Int -> Position -> Int
encodePosition w h ((x, y), r) = fromEnum r+6*(y+h*x)

decodePosition :: Int -> Int -> Int -> Position
decodePosition w h i = let (xy, r) = i `divMod` 6
                           (x, y) = xy `divMod` h
                       in ((x, y), toEnum r)

members :: Unit -> Position -> [(Int, Int)]
members u (p, r) = let pivotESE = u^.unitPivot.to toBaseESE
                   in u^.unitMembers&fmap (toBaseESE >>> subtract pivotESE >>> rotESE r >>> (+ pivotESE) >>> fromBaseESE >>> (+ p))

computeUnitData :: Int -> Int -> Unit -> Gr () Command
computeUnitData w h u = snd (execState (go (0, 0) RE) (mempty, mkGraph [] []))
  where
    allR = [minBound..maxBound] :: [Rotation]
    valid :: [(Int, Int)] -> Bool
    valid = getAll . foldMap (bifoldMap (\x -> All (x >= 0 && x < w)) (\y -> All (y >= 0 && y < h)))

    -- RecursiveDo !
    go :: (Int, Int) -> Rotation -> State (Set Position, Gr () Command) Bool
    go p@(x,y) r = do
      let e = encodePosition w h (p, r)
      b <- _1 . contains (p, r) <<.= True
      let v = valid (members u (p, r))
      when (v && not b) $ do
        _2 %= insNode (e, ())
        let psw = if y`mod`2 == 0 then (x, y+1) else (x+1, y+1)
        let pse = if y`mod`2 == 0 then (x-1, y+1) else (x, y+1)
        asw <- go psw r
        ase <- go pse r
        when asw (_2 %= insEdge (e, encodePosition w h (psw, r), MoveSW))
        when ase (_2 %= insEdge (e, encodePosition w h (pse, r), MoveSE))
      pure v

data SolverState = SolverState
                   { _stateRandom :: Integer
                   , _stateWidth  :: Int
                   , _stateHeight :: Int
                   , _stateUnits  :: Vector (Unit, Gr () Command)
                   , _stateGrid   :: Vector (Vector Bool)
                   }
makeLenses ''SolverState
type Solver a = StateT SolverState IO a

ldfWith :: Graph gr
           => CFun a b [(Node, c)]
           -> [(Node, c)]
           -> gr a b
           -> ([Tree (Node, c)],gr a b)
ldfWith _ []     g             = ([],g)
ldfWith _ _      g | isEmpty g = ([],g)
ldfWith d ((v, m):vs) g = case match v g of
  (Nothing,g1) -> ldfWith d vs g1
  (Just c,g1)  -> (Node (v, m) ts:ts',g3)
    where (ts, g2) = ldfWith d (d c) g1
          (ts', g3) = ldfWith d vs g2
ldffWith a b c = fst (ldfWith a b c)
plopTree :: Monoid c => c -> Tree (Node, c) -> [(Node, c)]
plopTree b (Node l f) = second (b <>) l : concat (plopTree (b <> (snd l)) <$> f)

solveOne :: Solver [Command]
solveOne = do
  n <- fmap ((.&. 0x7FFF) . flip shiftR 16 . fromIntegral) $ stateRandom <%= (`mod` (2^32)) . (+ 12345) . (* 1103515245)
  un <- use stateUnits
  v <- use stateGrid
  let (u, ugr) = un V.! (n `mod` V.length un)
  w <- use stateWidth
  h <- use stateHeight
  liftIO $ printMap (\i j -> v V.! i V.! j) w h
  liftIO $ putStrLn ""
  let rgr = ugr
            & nfilter (\(decodePosition w h -> p) -> all (\(i, j) -> not $ v V.! i V.! j) (members u p))
            & ldffWith (fmap (second (:[])) . lsuc') [(encodePosition w h ((0, 0), RE), [])]
            & concat . fmap (plopTree [])
  if null rgr
    then do
    pure []
    else do
    let (decodePosition w h -> ((x, y), r), c) = cycle rgr !! 42
        dt = members u ((x, y), r) & sort & groupBy ((==) `on` fst) & fmap (\xs@((x,_):_) -> (x, snd <$> xs))
    stateGrid %= (V.// (dt <&> \(x, y) -> (x, v V.! x V.// zip y (repeat True))))
    pure c

