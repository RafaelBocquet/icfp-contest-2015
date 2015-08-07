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

import Network.Wreq hiding (options, header, Options)
import System.Environment
import Options.Applicative
import Data.Aeson

import Vis
import Game
import Solver

data Options = Options
               { _optInput  :: [String]
               , _optTime   :: Maybe Int
               , _optMemory :: Maybe Int
               , _optPower  :: Maybe String
               , _optVis    :: String
               }
makeLenses ''Options

options :: Parser Options
options = Options
          <$> many (strOption
                    ( long "file"
                      <> short 'f'
                      <> metavar "FILE"
                      <> help "Input file" ))
          <*> optional (option auto ( short 't' ))
          <*> optional (option auto ( short 'm' ))
          <*> optional (strOption ( short 'p' ))
          <*> strOption
          ( long "vis"
            <> short 'v'
            <> help "Visualisation folder" )

printProblem :: Problem -> Vis ()
printProblem pb = do
  let w = pb ^. problemWidth
      h = pb ^. problemHeight
      f = pb ^. problemFilled & Set.fromList
  visLine ("SIZE : " ++ show (pb ^. problemWidth) ++ "x" ++ show (pb ^. problemHeight))
  visLine ("UNIT COUNT : " ++ show (pb ^. problemUnits.to length))
  visLine ("SOURCE LENGTH : " ++ show (pb ^. problemSourceLength))
  visLine ("SOURCE COUNT : " ++ show (pb ^. problemSourceSeeds.to length))
  liftIO $ putStrLn $ " === " ++ show (pb ^. problemId) ++ " === "
  forM_ [0..h-1] $ \i -> liftIO $ do
    when (i `mod` 2 /= 0) (putChar ' ')
    forM_ [0..w-1] $ \j -> do
      if Set.member (j, i) f
        then do
        setSGR [SetColor Foreground Vivid Red]
        putChar 'O'
        else putChar 'X'
      setSGR [Reset]
      putChar ' '
    putChar '\n'
  forM_ (pb ^. problemUnits) $ \u -> do
    visLine (show $ computeUnitData w h u)
    liftIO $ putStrLn "UNIT"
    let as = (u^.unitPivot):(u^.unitMembers)
    liftIO $ setSGR [SetColor Background Dull Green]
    liftIO $ forM_ [minimum (snd <$> as) .. maximum (snd <$> as)] $ \i -> do
      when (i `mod` 2 /= minimum (snd <$> as) `mod` 2) (putChar ' ')
      forM_ [minimum (fst <$> as) .. maximum (fst <$> as)] $ \j -> do
        when ((j, i) == u^.unitPivot) $ setSGR [SetColor Foreground Vivid Red]
        case ((j, i) == u^.unitPivot, (j, i) `elem` u^.unitMembers) of
          (False, False) -> putStr " "
          (True, False)  -> putStr "O"
          (_,    True)   -> putStr "X"
        setSGR [Reset]
        putChar ' '
      putChar '\n'


main :: IO ()
main = do
  options <- execParser $ info (helper <*> options)
             (fullDesc
              <> progDesc "ICFP 2015 !"
              <> header "Rafaël Bocquet & ???" )
  sol <- runVis (options ^. optVis) $ do
    forM (options ^. optInput) $ \inputfile -> do
      input     <- liftIO $ BL.readFile inputfile
      visLine ("Input file : " ++ inputfile)
      case decode input :: Maybe Problem of
        Nothing -> do
          visLine "Bad input"
          fail "Bad input"
        Just pb -> do
          printProblem pb
          forM (pb ^. problemSourceSeeds) $ \seed -> do
            pure $ Solution (pb ^. problemId) seed "" ""
  -- rsp <- postWith
  --        (defaults
  --         & auth .~ Just (basicAuth "" "dy5FWzIJnfSTL+RQ9J/7Xxk9s09GWCmybj6u+zbu8SE="))
  --        "https://davar.icfpcontest.org/teams/99/solutions"
  --        (toJSON $ concat sol)
  -- putStrLn $ "Answer : " ++ show rsp
  pure ()
