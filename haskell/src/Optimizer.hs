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

import Game

data Output a = OEmpty
              | OSingle a
              | OAppend (Output a) (Output a)
              | OAlt (Output a) (Output a)
              deriving (Show, Eq, Ord, Functor)

instance Applicative Output where pure = return; (<*>) = ap
instance Monad Output where
  return = OSingle
  OEmpty      >>= f = OEmpty
  OSingle x   >>= f = f x
  OAppend a b >>= f = OAppend (a >>= f) (b >>= f)
  OAlt a b    >>= f = OAlt (a >>= f) (b >>= f)

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

--


-- Map from AC State to max score
data OEntry a = OEntry { _oScore :: Int, _oList :: DList a }
            deriving (Show)

maxEntry :: OEntry a -> OEntry a -> OEntry a
maxEntry (OEntry a x) (OEntry b y) = if a >= b then OEntry a x else OEntry b y

appEntry :: OEntry a -> OEntry a -> OEntry a
appEntry (OEntry a x) (OEntry b y) = OEntry (a+b) (x<>y)

type OState a = IntMap (OEntry a)

oacNext :: Ord a => AC a -> Output a -> Int -> OState a
oacNext ac OEmpty      x = IntMap.singleton x (OEntry 0 DL.empty)
oacNext ac (OSingle a) x = let y = acNext ac x a
                           in IntMap.singleton y (OEntry (length (thisMatches ac y)) (DL.cons a DL.empty))
oacNext ac (OAppend a b) x = IntMap.toList (oacNext ac a x)
                             <&> (\(y, o) -> oacNext ac b y <&> appEntry o)
                             & IntMap.unionsWith maxEntry
oacNext ac (OAlt a b) x = IntMap.unionWith maxEntry (oacNext ac a x) (oacNext ac b x)

oacFromList :: [a] -> Output a
oacFromList = foldr OAppend OEmpty . fmap OSingle

--

outputString :: Output Command -> Output Char
outputString = (=<<) $ foldr1 OAlt . fmap OSingle
               . (\case
                      MoveW -> "p'!.03" :: String
                      MoveE -> "bcefy2"
                      MoveSW -> "aghij4"
                      MoveSE -> "lmno 5"
                      RotateCW -> "dqrvz1"
                      RotateCCW -> "kstuwx"
                 )

optimize :: Output Command -> String
optimize o = let x = oacNext (makeAC (makeTrie powerPhrases)) (outputString o) 0
             in x & IntMap.toList <&> snd & maximumBy (compare `on` _oScore) & _oList & DL.toList

powerPhrases :: [[Char]]
powerPhrases = [ "ei!"
               , "ia! ia!"
               , "r'lyeh"
               , "yuggoth"
               , "ph'nglui mglw'nafh cthulhu r'lyeh wgah'nagl fhtagn."
               , "blue hades"
               ]
