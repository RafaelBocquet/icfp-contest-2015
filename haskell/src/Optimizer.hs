module Optimizer where

import Prelude ()
import MyPrelude hiding (head, fromJust)
import Debug.Trace

import Data.List.Located (head)
import Data.Maybe.Located (fromJust)

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Diagrams.Prelude as D
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Data.DList as DL

data Output a = OEmpty
              | OCons a (Output a)
              | OAlt (Output a) (Output a)
              deriving (Show, Eq, Ord)

data Trie b a = Trie { _trieLabel :: b, _trieChildren :: [(a, Trie b a)] }
              deriving (Show)
makeLenses ''Trie

suffixes :: [a] -> [[a]]
suffixes []     = [[]]
suffixes (x:xs) = (x:xs):suffixes xs

treeOfTrie :: Trie b a -> Tree (b, [a])
treeOfTrie = go []
  where go a (Trie b ts) = Node (b, a) (ts <&> \(x, y) -> go (a++[x]) y)

makeTrie :: Ord a => [[a]] -> Trie (Bool, Int) a
makeTrie a = evalState (go False (sort a)) 0
  where go :: Ord a => Bool -> [[a]] -> State Int (Trie (Bool, Int) a)
        go b ([]:xs) = go True xs
        go b xs      = do
          i <- id <<+= 1
          Trie (b, i) <$> (forM (groupBy ((==) `on` head) xs) (\(xs@((z:_):_)) -> (z,) <$> go False (tail <$> xs)))

data ACStateData a = ACStateData
                     { _acSuffix   :: Int
                     , _acChildren :: Map a Int
                     , _acMatches  :: [Int]
                     }
makeLenses ''ACStateData
type AC a = (IntMap (ACStateData a), IntSet)

makeAC :: Ord a => Trie (Bool, Int) a -> AC a
makeAC a = let (b, c, d) = execState (go b c [] a) (mempty, mempty, mempty) in (c, d)
  where go :: Ord a => Map [a] Int -> IntMap (ACStateData a) ->
              [a] -> Trie (Bool, Int) a -> State (Map [a] Int, IntMap (ACStateData a), IntSet) ()
        go mp mp2 a (Trie (b, i) ts) = do
          _1.at a .= Just i
          when b (_3.contains i .= True)
          let suf = (if null a then i else head (catMaybes (fmap (lookup ?? mp) (tail (suffixes a)))))
          _2.at i .= Just (ACStateData
                           suf
                           (ts <&> second (snd . _trieLabel) & Map.fromList)
                           ((if b then (i :) else id) (if null a then [] else fromJust (lookup suf mp2) ^. acMatches))
                          )
          forM_ ts $ \(x, y) -> go mp mp2 (a++[x]) y

acNext :: Ord a => AC a -> Int -> a -> Int
acNext ac@(m, s) st x = do
  let Just v = lookup st m
  case lookup x (v^.acChildren) of
    Just i -> i
    Nothing -> if st == 0 then 0 else acNext ac (v^.acSuffix) x

runAC :: Ord a => AC a -> Int -> [a] -> [Int]
runAC ac x []     = [x]
runAC ac x (a:as) = x : runAC ac (acNext ac x a) as

thisMatches :: Ord a => AC a -> Int -> [Int]
thisMatches (m, _) x = fromJust (lookup x m) ^. acMatches

-- acThisMatches :: Ord a => AC a -> Int -> [Int]


--

-- type OState = IntMap Int

--

powerPhrases :: [[Char]]
powerPhrases = [ "ei!"
               -- , "ia! ia!"
               -- , "r'lyeh"
               -- , "yuggoth"
               -- , "ph'nglui mglw'nafh cthulhu r'lyeh wgah'nagl fhtagn."
               -- , "blue hades"
               ]
