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
import qualified Diagrams.Prelude as D
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Data.DList as DL

import Network.Wreq hiding (options, header, Options)
import System.Environment
import Options.Applicative
import Data.Aeson

import Game
import Solver
import Optimizer

data Options = Options
               { _optInput  :: [String]
               , _optTag    :: Maybe String
               , _optTime   :: Maybe Int
               , _optMemory :: Maybe Int
               , _optPower  :: [String]
               , _optSend   :: Bool
               -- , _optBranching :: Int
               -- , _optDepth     :: Int
               }
makeLenses ''Options

options :: Parser Options
options = Options
          <$> many (strOption
                    ( long "file"
                      <> short 'f'
                      <> metavar "FILE"
                      <> help "Input file" ))
          <*> optional (strOption ( long "tag" ))
          <*> optional (option auto ( short 't' ))
          <*> optional (option auto ( short 'm' ))
          <*> many (strOption ( short 'p' ))
          <*> switch ( long "send" )

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
  sol <- do
    forM (options ^. optInput) $ \inputfile -> do
      input     <- BL.readFile inputfile
      case decode input :: Maybe Problem of
        Nothing -> do
          fail "Bad input"
        Just pb -> do
          let units' = pb ^. problemUnits
                       <&> computeUnitData (pb^.problemWidth) (pb^.problemHeight)
                       & V.fromList
              initialMap = (V.generate (pb^.problemHeight)
                            (\i -> VU.generate (pb^.problemWidth)
                                   (\j -> Set.member (j, i) (pb^.problemFilled.to Set.fromList))))
          (scs, sol) <- fmap unzip
                        $ forM (zip (iterate (subtract 1) $ length (pb ^. problemSourceSeeds) - 1) $ pb ^. problemSourceSeeds)
                        $ \(seedi, seed) -> do
                          let w = pb ^. problemWidth
                          let h = pb ^. problemHeight
                          let localTime = time / fromIntegral (length $ options^.optInput) / fromIntegral (length $ pb^.problemSourceSeeds)
                                          / fromIntegral w / fromIntegral h
                          putStrLn $ "TIME : " ++ show localTime
                          let (branching, depth) =
                                find (\(a,b) -> fromIntegral (a^b) <= 140.0 * localTime)
                                (sortBy (flip compare `on` uncurry (^))
                                 $ [ (3,3), (3,4), (3,5), (3,6), (3, 7)
                                   , (4, 4), (4, 5), (4, 6), (4, 7)
                                   , (5, 5), (5, 6), (5, 7)
                                   , (6, 6), (6, 7)
                                   , (2,2), (2,3), (2,4), (2,5), (2,6)])
                                & maybe (1, 1) id
                          print (branching, depth)
                          let initialStep = SolveStep seed True initialMap 0 0 initialOState 0 0
                          let ac = makeAC (makeTrie powerPhrases)
                          let solveEnv = SolveEnv
                                         (pb^.problemWidth) (pb^.problemHeight)
                                         units'
                                         branching depth
                                         ac (oacCacheCommands ac)
                          let tree = runReader (solveTree initialStep) solveEnv
                          s <- runReaderT (pickOne (show seedi ++ " ") (pb^.problemSourceLength) tree) solveEnv
                          let opt = bestOState (s^.stepOState)
                          let sc = s^.stepScore + stateScore opt
                          print (opt&_oWhich)
                          putStrLn $ show (s^.stepScore) ++ " + " ++ show (stateScore opt)
                          -- liftIO $ print $ phraseToCommands (_oList opt & DL.toList)
                          -- liftIO $ simulate seed initialMap (pb^.problemWidth) (pb^.problemHeight) units'
                          --   (phraseToCommands (_oList opt & DL.toList))
                          pure $ (sc, Solution (pb ^. problemId) seed (fromMaybe "" (options ^. optTag)) (_oList opt & DL.toList))
          putStrLn $ "problem " ++ show (pb^.problemId) ++ " : " ++ show (sum scs `div` (length (pb ^. problemSourceSeeds)))
          pure sol
  when (options ^. optSend) $ do
    rsp <- postWith
           (defaults
            & auth .~ Just (basicAuth "" "dy5FWzIJnfSTL+RQ9J/7Xxk9s09GWCmybj6u+zbu8SE="))
           "https://davar.icfpcontest.org/teams/99/solutions"
           (toJSON $ concat sol)
    putStrLn $ "Answer : " ++ show rsp
  pure ()
