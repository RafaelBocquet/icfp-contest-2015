module Main where

import Prelude ()
import MyPrelude

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.DList as DL
import Data.Char

import Network.Wreq hiding (options, header, Options)
import System.Environment
import Options.Applicative
import Data.Aeson
import Control.Concurrent.ParallelIO

import Game
import Solver
import Optimizer

data Options = Options
               { _optInput  :: [String]
               , _optTag    :: Maybe String
               , _optTime   :: Maybe Int
               , _optMemory :: Maybe Int
               , _optCPUs :: Maybe Int
               , _optPower  :: [String]
               , _optSend   :: Bool
               , _optPrintMap :: Bool
               , _optInteractive :: Bool
               , _optDepth :: Maybe Int
               , _optBranching :: Maybe Int
               }
makeLenses ''Options

options :: Parser Options
options = Options
          <$> many (strOption
                    ( long "file"
                      <> short 'f'
                      <> metavar "FILE"
                      <> help "Input file" ))
          <*> optional (strOption ( long "tag" <> help "Submission time" ))
          <*> optional (option auto ( short 't' <> help "Time limit" ))
          <*> optional (option auto ( short 'm' <> help "Available memory" ))
          <*> optional (option auto ( short 'c' <> help "Number of available cores" ))
          <*> many (strOption ( short 'p' <> help "Power phrase" ))
          <*> switch ( long "send" <> help "Send the solution to the leaderboard")
          <*> switch ( long "print-map" )
          <*> switch ( long "interactive" )
          <*> optional (option auto (short 'd' <> help "Solve tree search depth"))
          <*> optional (option auto (short 'b' <> help "Solve tree branching factor"))

printProblem :: Problem -> IO ()
printProblem pb = do
  let w = pb ^. problemWidth
      h = pb ^. problemHeight
      f = pb ^. problemFilled & Set.fromList
  putStrLn $ " === " ++ show (pb ^. problemId) ++ " === "
  printMap (curry $ Set.member ?? f) w h
  forM_ (pb ^. problemUnits) $ \u -> do
    liftIO $ putStrLn "UNIT"
    liftIO $ printUnit $ u

stringOfCommands :: [Command] -> String
stringOfCommands = fmap (head . tail . (\case
                                            MoveW -> "p'!.03"
                                            MoveE -> "bcefy2"
                                            MoveSW -> "aghij4"
                                            MoveSE -> "lmno 5"
                                            RotateCW -> "dqrvz1"
                                            RotateCCW -> "kstuwx"))


main :: IO ()
main = do
  options <- execParser $ info (helper <*> options)
             (fullDesc
              <> progDesc "ICFP 2015 !"
              <> header "Rafaël Bocquet & ???" )
  let time = fromIntegral $ fromMaybe 500 (options ^. optTime) :: Double
  let powerPhrases = if null (options^.optPower)
                     then [ "ei!"
                          , "ia! ia!"
                          , "r'lyeh"
                          , "yuggoth"
                          , "tsathoggua"
                          , "yogsothoth"
                          , "necronomicon"
                          , "vigintillion"
                          , "cthulhu fhtagn!"
                          , "ph'nglui mglw'nafh cthulhu r'lyeh wgah'nagl fhtagn."
                          , "in his house at r'lyeh dead cthulhu waits dreaming."
                          , "the laundry"
                          , "planet 10"
                          , "yoyodyne"
                          , "monkeyboy"
                          , "john bigboote"
                          , "blue hades"
                          , "case nightmare green"
                          ]
                     else (fmap (fmap toLower) $ options^.optPower)
  sol <- do
    sequence $ (flip fmap) (options ^. optInput) $ \inputfile -> do
      input     <- BL.readFile inputfile
      case decode input :: Maybe Problem of
        Nothing -> fail "Bad input"
        Just pb -> do
          let units' = pb ^. problemUnits
                       <&> computeUnitData (pb^.problemWidth) (pb^.problemHeight)
                       & V.fromList
              initialMap = (V.generate (pb^.problemHeight)
                            (\i -> foldl' (\a j -> if Set.member (j, i) (pb^.problemFilled.to Set.fromList) then setBit a j else a)
                                   0 [0..pb^.problemWidth-1]))
          (scs, sol) <- fmap unzip
                        $ sequence
                        $ (flip fmap) (zip (iterate (subtract 1) $ length (pb ^. problemSourceSeeds) - 1) $ pb ^. problemSourceSeeds)
                        $ \(seedi, seed) -> do
                          let w = pb ^. problemWidth
                          let h = pb ^. problemHeight
                          let localTime = time / fromIntegral (length $ options^.optInput)
                                          / fromIntegral (length $ pb^.problemSourceSeeds)
                                          / fromIntegral (pb^.problemSourceLength)
                                          / fromIntegral w / fromIntegral h
                                          / fromIntegral (sum (length <$> powerPhrases))
                          putStrLn $ "TIME : " ++ show localTime
                          let (branching, depth) =
                                find (\(a,b) -> fromIntegral (a^b) <= 1500000.0 * localTime)
                                (sortBy (flip compare `on` uncurry (^))
                                 $ [ (3,3), (3,4), (3,5), (3, 6), (3, 7), (3, 8)
                                   , (2,2), (2,3), (2,4), (2, 5)])
                                & maybe (1, 1) id
                          hPrint stderr (branching, depth)
                          let initialStep = SolveStep seed True initialMap 0 0 0 initialOState
                          let ac = makeAC (makeTrie powerPhrases)
                          let solveEnv = SolveEnv
                                         (pb^.problemWidth) (pb^.problemHeight)
                                         units'
                                         (fromMaybe branching $ options^.optBranching) (fromMaybe depth $ options^.optDepth)
                                         ac (oacCacheCommands ac)
                          let tree = runReader (solveTree initialStep) solveEnv
                          s <- runReaderT (pickOne
                                           (options^.optPrintMap)
                                           (options^.optInteractive)
                                           (show seedi ++ " ")
                                           (pb^.problemSourceLength)
                                           tree) solveEnv
                          let opt = bestOState (s^.stepOState)
                          let sc = s^.stepScore + stateScore opt
                          hPrint stderr (opt&_oWhich)
                          hPutStrLn stderr $ show (s^.stepScore) ++ " + " ++ show (stateScore opt :: Int)
                          hPutStrLn stderr $ show (fromIntegral (stateScoreSimple opt) / 2
                                                   / fromIntegral (length (_oList opt & DL.toList)) :: Double)
                          -- liftIO $ print $ phraseToCommands (_oList opt & DL.toList)
                          -- liftIO $ simulate seed initialMap (pb^.problemWidth) (pb^.problemHeight) units'
                          --   (phraseToCommands (_oList opt & DL.toList))
                          pure $ (sc, Solution (pb ^. problemId) seed (fromMaybe "" (options ^. optTag)) (_oList opt & DL.toList))
          hPutStrLn stderr $ "problem " ++ show (pb^.problemId) ++ " : " ++ show (sum scs `div` (length (pb ^. problemSourceSeeds)))
          pure sol
  if (options ^. optSend)
    then do
    rsp <- postWith
           (defaults
            & auth .~ Just (basicAuth "" "dy5FWzIJnfSTL+RQ9J/7Xxk9s09GWCmybj6u+zbu8SE="))
           "https://davar.icfpcontest.org/teams/99/solutions"
           (toJSON $ concat sol)
    putStrLn $ "Answer : " ++ show rsp
    else BL.putStrLn (encode (toJSON (concat sol)))
  stopGlobalPool
