module Solver where

import Prelude ()
import MyPrelude
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

instance Num (Int, Int) where
  (a, b) + (x, y) = (a+x, b+y)
  (a, b) - (x, y) = (a-x, b-y)
  negate (x, y)   = (-x, -y)

-- UnitState = (Int, Int) -> ((Int, Int) -> Bool) -> Maybe [Command]
-- How to compute efficiently ?

computeUnitData :: Int -> Int -> Unit -> _
computeUnitData w h u = execState (go (0, 0) RE) mempty
  where
    allR = [minBound..maxBound] :: [Rotation]
    pivotESE = u ^. unitPivot.to toBaseESE
    toBaseESE (x, y) = (x - y `div` 2, y)
    fromBaseESE (x, y) = (x + y `div` 2, y)
    rotESE :: Rotation -> (Int, Int) -> (Int, Int)
    rotESE RE (x, y) = (x, y)
    rotESE RSE (x, y) = (-y, x+y)
    rotESE RSW (x, y) = (-x-y, -x)
    rotESE RW (x, y) = (-x, -y)
    rotESE RNW (x, y) = (y, -x-y)
    rotESE RNE (x, y) = (x+y, -x)

    members :: Rotation -> [(Int, Int)]
    members r = u^.unitMembers&fmap (toBaseESE >>> subtract pivotESE >>> rotESE r >>> (+ pivotESE) >>> fromBaseESE)

    valid :: (Int, Int) -> Rotation -> Bool
    valid p r = traceShow (p, r) $ getAll (foldMap (bifoldMap (\x -> All (x >= 0 && x < w)) (\y -> All (y >= 0 && y < h))) ((+p) <$> members r))

    -- RecursiveDo !
    go :: (Int, Int) -> Rotation -> State (Map ((Int, Int), Rotation) _) (Maybe (Maybe _))
    go p@(x,y) r = mdo
      a <- at (p, r) <%= Just . maybe s id
      s <- if valid p r
           then do
             let psw = if y`mod`2 == 0 then (x, y+1) else (x+1, y+1)
             let pse = if y`mod`2 == 0 then (x-1, y+1) else (x, y+1)
             asw <- go psw r
             ase <- go pse r
             pure (Just ())
           else pure Nothing
      pure a
