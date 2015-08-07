{-# LANGUAGE OverloadedStrings #-}
module Game where

import Prelude ()
import MyPrelude
import Data.Aeson

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Diagrams.Prelude as D
import qualified Graphics.Rendering.Chart.Easy as C


data Problem = Problem
               { _problemId :: Integer
               , _problemUnits :: [Unit]
               , _problemWidth :: Int
               , _problemHeight :: Int
               , _problemFilled :: [(Int, Int)]
               , _problemSourceLength :: Integer
               , _problemSourceSeeds :: [Integer]
               }
               deriving (Show)

data Cell = Cell { _cellX :: Int, _cellY :: Int }
            deriving (Show, Eq, Ord)
pairOfCell (Cell x y) = (x, y)

data Unit = Unit
            { _unitMembers :: [(Int, Int)]
            , _unitPivot :: (Int, Int)
            }
            deriving (Show)

data Command = MoveW | MoveE | MoveSW | MoveSE | RotateCW | RotateCCW

instance FromJSON Problem where
  parseJSON (Object v) = Problem
                         <$> v .: "id"
                         <*> v .: "units"
                         <*> v .: "width"
                         <*> v .: "height"
                         <*> (fmap pairOfCell <$> v .: "filled")
                         <*> v .: "sourceLength"
                         <*> v .: "sourceSeeds"
  parseJSON _ = error "parseJSON: Problem"

instance FromJSON Cell where
  parseJSON (Object v) = Cell
                         <$> v .: "x"
                         <*> v .: "y"
  parseJSON _ = error "parseJSON: Cell"

instance FromJSON Unit where
  parseJSON (Object v) = Unit
                         <$> (fmap pairOfCell <$> v .: "members")
                         <*> (pairOfCell <$> v .: "pivot")
  parseJSON _ = error "parseJSON: Unit"

makeLenses ''Problem
makeLenses ''Cell
makeLenses ''Unit
