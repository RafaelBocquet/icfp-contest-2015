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
import Data.Aeson as A

data Solution = Solution
                { _solutionProblemId :: Int
                , _solutionSeed      :: Int
                , _solutionScore    :: Int
                , _solutionTag       :: String
                , _solutionCommands  :: String
                }

instance ToJSON Solution where
  toJSON (Solution a b _ c d) = object [ "problemId" A..= a
                                       , "seed" A..= b
                                       , "tag" A..= c
                                       , "solution" A..= d
                                       ]

data SolutionIn = SolutionIn
                { _sProblem :: Int
                , _sSeed    :: Int
                , _sScore   :: Int
                , _sData    :: String
                }

instance FromJSON SolutionIn where
  parseJSON (Object v) = SolutionIn
                         <$> v .: "problemId"
                         <*> v .: "seed"
                         <*> (fromMaybe (-10000000) <$> (v .:? "score"))
                         <*> v .: "solution"
  parseJSON _ = error "parseJSON: Problem"

main :: IO ()
main = do
  [tag, fn] <- getArgs
  rsp1 <- BL.readFile fn
  print "received"
  let a = eitherDecode rsp1 :: Either String [SolutionIn]
  case a of
    Left s -> print s
    Right a -> do
      let b = a <&> (\s -> ((s&_sProblem, s&_sSeed), (s&_sScore, Solution (s&_sProblem) (s&_sSeed) (s&_sScore) tag (s&_sData))))
              & Map.fromListWith (\(a,x) (b,y) -> if a >= b then (a, x) else (b,y))
              & Map.elems <&> snd & groupBy ((==) `on` _solutionProblemId)
      forM_ b $ \pbs -> do
        putStrLn $ show (head pbs&_solutionProblemId) ++ "\t" ++ show (sum (pbs<&>_solutionScore)`div`length pbs)
      print (length (show (toJSON b)))
      rsp <- postWith
             (defaults
              & auth .~ Just (basicAuth "" "dy5FWzIJnfSTL+RQ9J/7Xxk9s09GWCmybj6u+zbu8SE="))
             "https://davar.icfpcontest.org/teams/99/solutions"
             (toJSON (concat b))
      print rsp
