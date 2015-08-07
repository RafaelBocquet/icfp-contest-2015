module Solver where

import Prelude ()
import MyPrelude

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

computeUnitData :: Int -> Int -> Unit -> _
computeUnitData w h u = zip allR (members <$> allR)
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
