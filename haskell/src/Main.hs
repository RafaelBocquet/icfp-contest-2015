module Main where

import Prelude ()
import MyPrelude

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

import System.Environment
import Options.Applicative
import Data.Aeson

import Vis
import Game

data Options = Options
               { _optInput  :: String
               , _optVis    :: String
               }
makeLenses ''Options

options :: Parser Options
options = Options
          <$> strOption
          ( long "input"
            <> short 'i'
            <> metavar "INPUT"
            <> help "Input file" )
          <*> strOption
          ( long "vis"
            <> short 'v'
            <> help "Visualisation folder" )

main :: IO ()
main = do
  options <- execParser $ info (helper <*> options)
             (fullDesc
              <> progDesc "ICFP 2015 !"
              <> header "Rafaël Bocquet & ???" )
  input     <- BL.readFile (options ^. optInput)
  runVis (options ^. optVis) $ do
    visLine ("Input file : " ++ options ^. optInput)
    case decode input :: Maybe Problem of
      Nothing -> do
        visLine "Bad input"
        fail "Bad input"
      Just pb -> do
        let w = pb ^. problemWidth
            h = pb ^. problemHeight
            f = pb ^. problemFilled & Set.fromList
        visLine ("SIZE : " ++ show (pb ^. problemWidth) ++ "x" ++ show (pb ^. problemHeight))
        visLine ("UNIT COUNT : " ++ show (pb ^. problemUnits.to length))
        forM_ [0..h-1] $ \i -> liftIO $ do
          when (i `mod` 2 /= 0) (putChar ' ')
          forM_ [0..w-1] $ \j -> do
            if Set.member (i, j) f
              then do
              setSGR [SetColor Foreground Vivid Red]
              putChar 'O'
              else putChar 'X'
            setSGR [Reset]
            putChar ' '
          putChar '\n'
