module Main where

import Prelude ()
import MyPrelude

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

import System.Environment
import Options.Applicative

import Vis

data Options = Options
               { _optInput  :: Either String String
               , _optOutput :: String
               , _optVis    :: String
               }
makeLenses ''Options

options :: Parser Options
options = Options
          <$> (Left <$> strOption
               ( long "input"
                 <> short 'i'
                 <> metavar "INPUT"
                 <> help "Input file" )
               <|> (Right <$> strOption
                    ( long "inputstring"
                    <> metavar "INPUTSTRING"
                    <> help "Input string" )))
          <*> strOption
          ( long "output"
            <> short 'o'
            <> help "Output file" )
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
  input     <- either readFile pure (options ^. optInput)
  runVis (options ^. optVis) $ do
    visDiagram ((D.unitCircle D.||| D.unitCircle) & D.center)
    visChart $ do
      let signal :: [Double] -> [(Double, Double)]
          signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]
      C.layout_title .= "Amplitude Modulation"
      C.setColors [C.opaque C.blue, C.opaque C.red]
      C.plot (C.line "am" [signal [0,(0.5)..400]])
      C.plot (C.points "am points" (signal [0,7..400]))
    pure ()
