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
                , _solutionTag       :: String
                , _solutionCommands  :: String
                }

instance ToJSON Solution where
  toJSON (Solution a b c d) = object [ "problemId" A..= a
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
                         <*> v .: "score"
                         <*> v .: "solution"
  parseJSON _ = error "parseJSON: Problem"

main :: IO ()
main = do
  [tag] <- getArgs
  rsp1 <- getWith
         (defaults
          & auth .~ Just (basicAuth "" "dy5FWzIJnfSTL+RQ9J/7Xxk9s09GWCmybj6u+zbu8SE="))
         "https://davar.icfpcontest.org/teams/99/solutions"
  let Just a = decode (rsp1^.responseBody) :: Maybe [SolutionIn]
      b = a <&> (\s -> ((s&_sProblem, s&_sSeed), (s&_sScore, Solution (s&_sProblem) (s&_sSeed) tag (s&_sData))))
          & Map.fromListWith (\(a,x) (b,y) -> if a >= b then (a, x) else (b,y))
          & Map.elems <&> snd
  rsp <- postWith
         (defaults
          & auth .~ Just (basicAuth "" "dy5FWzIJnfSTL+RQ9J/7Xxk9s09GWCmybj6u+zbu8SE="))
         "https://davar.icfpcontest.org/teams/99/solutions"
         (toJSON b)
  print rsp
