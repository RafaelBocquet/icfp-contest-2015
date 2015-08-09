{-# LANGUAGE OverloadedStrings #-}
module Game where

import Prelude ()
import MyPrelude
import Data.Aeson as A

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
               , _problemSourceLength :: Int
               , _problemSourceSeeds :: [Int]
               }
               deriving (Show)

data Cell = Cell { _cellX :: Int, _cellY :: Int }
            deriving (Show, Eq, Ord)

pairOfCell ∷ Cell → (Int, Int)
pairOfCell (Cell x y) = (x, y)

data Unit = Unit
            { _unitMembers :: [(Int, Int)]
            , _unitPivot :: (Int, Int)
            }
            deriving (Show)

data Command = MoveW | MoveE | MoveSW | MoveSE | RotateCW | RotateCCW
             deriving (Eq, Ord, Show, Generic)
instance Hashable Command
instance NFData Command

data Solution = Solution
                { _solutionProblemId :: Integer
                , _solutionSeed      :: Int
                , _solutionTag       :: String
                , _solutionCommands  :: String
                }

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

instance ToJSON Solution where
  toJSON (Solution a b c d) = object [ "problemId" A..= a
                                     , "seed" A..= b
                                     , "tag" A..= c
                                     , "solution" A..= d
                                     ]

makeLenses ''Problem
makeLenses ''Cell
makeLenses ''Unit


printMap :: (Int -> Int -> Bool) -> Int -> Int -> IO ()
printMap v w h = do
  forM_ [0..h-1] $ \i -> do
    when (i `mod` 2 /= 0) (putChar ' ')
    forM_ [0..w-1] $ \j -> do
      if v j i
        then do
        setSGR [SetColor Foreground Vivid Red]
        putChar 'O'
        else putChar 'X'
      setSGR [Reset]
      putChar ' '
    putChar '\n'

printUnit :: Unit -> IO ()
printUnit u = do
  let as = (u^.unitPivot):(u^.unitMembers)
  forM_ [minimum (snd <$> as) .. maximum (snd <$> as)] $ \i -> do
    when (i `mod` 2 /= 0) (putChar ' ')
    forM_ [minimum (fst <$> as) .. maximum (fst <$> as)] $ \j -> do
      when ((j, i) == u^.unitPivot) $ setSGR [SetColor Foreground Vivid Red]
      case ((j, i) == u^.unitPivot, (j, i) `elem` u^.unitMembers) of
        (False, False) -> putStr " "
        (True, False)  -> putStr "O"
        (_,    True)   -> putStr "X"
      setSGR [Reset]
      putChar ' '
    putChar '\n'
