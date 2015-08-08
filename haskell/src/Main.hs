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

import Network.Wreq hiding (options, header, Options)
import System.Environment
import Options.Applicative
import Data.Aeson

import Vis
import Game
import Solver

data Options = Options
               { _optInput  :: [String]
               , _optTag    :: Maybe String
               , _optTime   :: Maybe Int
               , _optMemory :: Maybe Int
               , _optPower  :: Maybe String
               , _optSend   :: Bool
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
          <*> optional (strOption ( long "tag" ))
          <*> optional (option auto ( short 't' ))
          <*> optional (option auto ( short 'm' ))
          <*> optional (strOption ( short 'p' ))
          <*> switch ( long "send" )
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
  liftIO $ printMap (curry $ Set.member ?? f) w h
  forM_ (pb ^. problemUnits) $ \u -> do
    visLine (show $ computeUnitData w h u)
    liftIO $ putStrLn "UNIT"
    liftIO $ forM_ [minBound..maxBound] $ \r -> do
      printUnit $ u & unitMembers .~ members u ((0, 0), r)
      liftIO $ putStrLn "==="
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
  sol <- runVis (options ^. optVis) $ do
    forM (options ^. optInput) $ \inputfile -> do
      input     <- liftIO $ BL.readFile inputfile
      visLine ("Input file : " ++ inputfile)
      case decode input :: Maybe Problem of
        Nothing -> do
          visLine "Bad input"
          fail "Bad input"
        Just pb -> do
          -- printProblem pb
          let units' = pb ^. problemUnits
                       <&> (\x -> (x, computeUnitData (pb^.problemWidth) (pb^.problemHeight) x))
                       & V.fromList
          forM (pb ^. problemSourceSeeds) $ \seed -> do
            let initState = SolverState True seed (pb^.problemWidth) (pb^.problemHeight)
                            units'
                            (V.generate (pb^.problemHeight)
                             (\i -> VU.generate (pb^.problemWidth)
                                    (\j -> Set.member (j, i) (pb^.problemFilled.to Set.fromList))))
                            []
                            0
                            0
                            0
            let tree = stateTree initState
            c <- liftIO $ pickOne (pb^.problemSourceLength) tree <&> (^. stateCommands)
            pure $ Solution (pb ^. problemId) seed (fromMaybe "" (options ^. optTag)) (stringOfCommands c)
  print (encode (toJSON sol))
  when (options ^. optSend) $ do
    rsp <- postWith
           (defaults
            & auth .~ Just (basicAuth "" "dy5FWzIJnfSTL+RQ9J/7Xxk9s09GWCmybj6u+zbu8SE="))
           "https://davar.icfpcontest.org/teams/99/solutions"
           (toJSON $ concat sol)
    putStrLn $ "Answer : " ++ show rsp
  pure ()
