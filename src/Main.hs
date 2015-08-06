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

import System.Environment
import Options.Applicative

data Options = Options
               { _optInput  :: Either String String
               , _optOutput :: String
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

main :: IO ()
main = do
  options <- execParser $ info (helper <*> options)
             (fullDesc
              <> progDesc "ICFP 2015 !"
              <> header "Rafaël Bocquet & ???" )
  input     <- either readFile pure (options ^. optInput)
  pure ()
