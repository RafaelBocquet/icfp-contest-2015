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




computeUnitData :: Int -> Int -> Unit -> _
computeUnitData w h u = snd (execState (go (0, 0) RE) (mempty, mkGraph [] [] :: Gr _ _))
  where
    allR = [minBound..maxBound] :: [Rotation]
    pivotESE = u ^. unitPivot.to toBaseESE
    members :: Rotation -> [(Int, Int)]
    members r = u^.unitMembers&fmap (toBaseESE >>> subtract pivotESE >>> rotESE r >>> (+ pivotESE) >>> fromBaseESE)

    valid :: [(Int, Int)] -> Bool
    valid = getAll . foldMap (bifoldMap (\x -> All (x >= 0 && x < w)) (\y -> All (y >= 0 && y < h)))

    -- RecursiveDo !
    go :: (Int, Int) -> Rotation -> State (Set ((Int, Int), Rotation), Gr () Command) Bool
    go p@(x,y) r = do
      let e = encodePosition w h (p, r)
      b <- _1 . contains (p, r) <<.= True
      let v = valid ((p +) <$> members r)
      when (v && not b) $ do
        _2 %= insNode (e, ())
        let psw = if y`mod`2 == 0 then (x, y+1) else (x+1, y+1)
        let pse = if y`mod`2 == 0 then (x-1, y+1) else (x, y+1)
        asw <- go psw r
        ase <- go pse r
        when asw (_2 %= insEdge (e, encodePosition w h (psw, r), MoveSW))
        when ase (_2 %= insEdge (e, encodePosition w h (pse, r), MoveSE))
      pure v

-- type PosLookup = (Int, Int) -> Bool
-- type UnitData = Map ((Int, Int), Rotation) (PosLookup -> Maybe [Command])

-- computeUnitData :: Int -> Int -> Unit -> _
-- computeUnitData w h u = execState (go (0, 0) RE) mempty
--   where
--     allR = [minBound..maxBound] :: [Rotation]
--     pivotESE = u ^. unitPivot.to toBaseESE
--     members :: Rotation -> [(Int, Int)]
--     members r = u^.unitMembers&fmap (toBaseESE >>> subtract pivotESE >>> rotESE r >>> (+ pivotESE) >>> fromBaseESE)

--     valid :: [(Int, Int)] -> Bool
--     valid = getAll . foldMap (bifoldMap (\x -> All (x >= 0 && x < w)) (\y -> All (y >= 0 && y < h)))

--     kalt a b = \c -> a c <|> b c

--     -- RecursiveDo !
--     go :: (Int, Int) -> Rotation -> State (Map ((Int, Int), Rotation) UnitData) UnitData
--     go p@(x,y) r = mdo
--       ~(Just a) <- at (p, r) <%= Just . maybe s id
--       let cpos = (+p) <$> members r
--       s <- if valid cpos
--            then do
--              let psw = if y`mod`2 == 0 then (x, y+1) else (x+1, y+1)
--              let pse = if y`mod`2 == 0 then (x-1, y+1) else (x, y+1)
--              asw <- go psw r
--              ase <- go pse r
--              -- pure $ Map.insertWith kalt (p, r) (\v -> do
--              --                                        guard (all v cpos)
--              --                                        (do
--              --                                             guard (not (all v ((+psw)<$>members r)))
--              --                                             pure (Map.singleton (p, r) [MoveSW])) <|>
--              --                                          (do
--              --                                               guard (not (all v ((+pse)<$>members r)))
--              --                                               pure (Map.singleton (p, r) [MoveSE]))
--              --                                   )
--              pure $ Map.insertWith kalt (p, r) (\v -> do
--                                                     guard (all v cpos)
--                                                     (do guard (not (all v ((+psw)<$>))))
--                                                ) (Map.unionWith kalt asw ase)
--            else pure mempty
--       pure a
