{-# LANGUAGE OverloadedStrings #-}
module Game where

import Prelude ()
import MyPrelude
import Data.Aeson

data Problem = Problem
               { _problemId :: Integer
               , _problemUnits :: [Unit]
               , _problemWidth :: Int
               , _problemHeight :: Int
               , _problemFilled :: [Cell]
               , _problemSourceLength :: Integer
               , _problemSourceSeeds :: [Integer]
               }
               deriving (Show)

data Cell = Cell { _cellX :: Int, _cellY :: Int }
            deriving (Show, Eq, Ord)

data Unit = Unit
            { _unitMembers :: [Cell]
            , _unitPivot :: Cell
            }
            deriving (Show)

instance FromJSON Problem where
  parseJSON (Object v) = Problem
                         <$> v .: "id"
                         <*> v .: "units"
                         <*> v .: "width"
                         <*> v .: "height"
                         <*> v .: "filled"
                         <*> v .: "sourceLength"
                         <*> v .: "sourceSeeds"

instance FromJSON Cell where
  parseJSON (Object v) = Cell
                         <$> v .: "x"
                         <*> v .: "y"

instance FromJSON Unit where
   parseJSON (Object v) = Unit
                          <$> v .: "members"
                          <*> v .: "pivot"

makeLenses ''Problem
makeLenses ''Cell
makeLenses ''Unit
