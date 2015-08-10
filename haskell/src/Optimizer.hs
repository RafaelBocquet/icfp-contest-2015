module Optimizer where

import Prelude ()
import MyPrelude
import Debug.Trace


import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.DList as DL

import Game

data Output a = OEmpty
              | OSingle a
              | OAppend (Output a) (Output a)
              | OAlt (Output a) (Output a)
              deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
instance Hashable a => Hashable (Output a)


outputSize :: Output a -> Integer
outputSize = getSum . foldMap (const (Sum 1))

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

-- AC

data ACStateData = ACStateData
                   { _acSuffix     :: Int
                   , _acChildren   :: Vector (Maybe Int)
                   , _acMatches    :: Int
                   , _acMatchesHow :: IntSet
                   }
makeLenses ''ACStateData
type AC = (Int, Vector ACStateData, IntSet)

makeAC :: Trie (Bool, Int) Char -> AC
makeAC a = let (b, c, d) = execState (go b c [] a) (mempty, mempty, mempty) in ( fromJust (IntMap.maxViewWithKey c)^._1._1
                                                                               , V.fromList (IntMap.elems c)
                                                                               , d)
  where go :: Map [Char] Int -> IntMap ACStateData ->
              [Char] -> Trie (Bool, Int) Char -> State (Map [Char] Int, IntMap ACStateData, IntSet) ()
        go mp mp2 a (Trie (b, i) ts) = do
          _1.at a .= Just i
          when b (_3.contains i .= True)
          let suf = (if null a then i else head (catMaybes (fmap (lookup ?? mp) (tail (suffixes a)))))
          _2.at i .= Just (ACStateData
                           suf
                           (let cs = ts <&> bimap fromEnum (Just . snd . _trieLabel)
                            in V.zipWith (<|>)
                               (V.replicate 256 Nothing V.// cs)
                               (if null a
                                then V.replicate 256 Nothing
                                else fromJust (lookup suf mp2) ^. acChildren))
                           ((if b then (length a +) else id) (if null a then 0 else fromJust (lookup suf mp2) ^. acMatches))
                           ((if b then IntSet.insert i else id) (if null a then IntSet.empty else fromJust (lookup suf mp2) ^. acMatchesHow))
                          )
          forM_ ts $ \(x, y) -> go mp mp2 (a++[x]) y

acNext :: AC -> Int -> Char -> Int
acNext ac@(_, m, s) st x = do
  let v = m V.! st
  case (v^.acChildren) V.! fromEnum x of
    Just i -> i
    Nothing -> if st == 0 then 0 else acNext ac (v^.acSuffix) x

runAC :: AC -> Int -> [Char] -> [Int]
runAC ac x []     = [x]
runAC ac x (a:as) = x : runAC ac (acNext ac x a) as

thisMatches :: AC -> Int -> Int
thisMatches (_, m, _) x = (m V.! x) ^. acMatches

thisMatchesHow :: AC -> Int -> IntSet
thisMatchesHow (_, m, _) x = (m V.! x) ^. acMatchesHow

--


-- Map from AC State to max score
data OEntry a = OEntry { _oScore :: Int, _oWhich :: IntSet, _oList :: DList a }
              deriving (Show)

{-# INLINE maxEntry #-}
maxEntry :: OEntry a -> OEntry a -> OEntry a
maxEntry (OEntry a w x) (OEntry b z y) = if a >= b then OEntry a w x else OEntry b z y

type OState a = IntMap (OEntry a)

{-# INLINE maxOState #-}
maxOState :: OState a -> OState a -> OState a
maxOState = IntMap.unionWith maxEntry

{-# INLINE appEntry #-}
appEntry :: Ord a => OEntry a -> OEntry a -> OEntry a
appEntry (OEntry a w x) (OEntry b z y) = OEntry (a+b) (IntSet.union w z) (x<>y)

oacCache :: Vector (OState Char) -> OState Char -> OState Char
oacCache v x = x & IntMap.toList
               <&> (\(y, o) -> v V.! y <&> appEntry o)
               & IntMap.unionsWith maxEntry

oacMakeCache :: AC -> Output Char -> Vector (OState Char)
oacMakeCache ac@(s,_,_) o = V.generate (s+1) $ \i -> oacNext ac o (IntMap.singleton i (OEntry 0 mempty mempty))

-- type OCacheT = StateT (HashMap (Output Char) (Vector (OState Char)))

-- oacCacheOutput :: (Monad m, MonadFix m) => AC -> Output Char -> OCacheT m (Vector (OState Char))
-- oacCacheOutput ac o = mdo
--   a <- (at o <.= Just a)
--        >>= maybe (pure (oacMakeCache ac o)) pure
--   pure a

-- oacCacheNext :: (Monad m, MonadFix m) => AC -> Output Char -> OState Char -> OCacheT m (OState Char)
-- oacCacheNext ac o x = oacCache <$> oacCacheOutput ac o <*> pure x

oacNext :: AC -> Output Char -> OState Char -> OState Char
oacNext ac OEmpty      x = x
oacNext ac (OSingle a) x = x & IntMap.toList
                           <&> (\(y, o) -> let z = acNext ac y a
                                           in (z, o & flip appEntry (OEntry (thisMatches ac y) (thisMatchesHow ac y) (DL.cons a DL.empty))))
                           & IntMap.fromListWith maxEntry
oacNext ac (OAppend a b) x = oacNext ac b (oacNext ac a x)
oacNext ac (OAlt a b) x = maxOState (oacNext ac a x) (oacNext ac b x)

oacFromList :: [a] -> Output a
oacFromList = foldr OAppend OEmpty . fmap OSingle

--

outputString :: Output Command -> Output Char
outputString = (=<<) $ foldr1 OAlt . fmap OSingle
               . (\case
                      MoveW      -> "p'!.03" :: String
                      MoveE      -> "bcefy2"
                      MoveSW     -> "aghij4"
                      MoveSE     -> "lmno 5"
                      RotateCW   -> "dqrvz1"
                      RotateCCW  -> "kstuwx"
                 )

-- optimize :: Output Command -> OEntry Char
-- optimize o = bestOState (oacNext (makeAC (makeTrie powerPhrases)) (outputString o) initialOState)

initialOState :: OState a
initialOState = IntMap.singleton 0 (OEntry 0 IntSet.empty DL.empty)

bestOState :: OState a -> OEntry a
bestOState x = x & IntMap.toList <&> snd & maximumBy (compare `on` _oScore)

stateScore :: Integral b => OEntry a -> b
stateScore opt = 2 * fromIntegral (_oScore opt) + 300 * fromIntegral (IntSet.size (_oWhich opt))

stateScoreSimple :: Integral b => OEntry a -> b
stateScoreSimple opt = 2 * fromIntegral (_oScore opt)

phraseToCommands ∷ String → [Command]
phraseToCommands = fmap (\x -> fst . fromJust $ find (elem x . snd)
                               [ (MoveW, "p'!.03" :: String)
                               , (MoveE , "bcefy2")
                               , (MoveSW , "aghij4")
                               , (MoveSE , "lmno 5")
                               , (RotateCW , "dqrvz1")
                               , (RotateCCW , "kstuwx") ] )

type OCommandCache = [Command] -> Vector (OState Char)

oacCacheCommands :: AC -> [Command] -> Vector (OState Char)
oacCacheCommands ac = let m = Map.fromList
                              $ [MoveW, MoveE, MoveSW, MoveSE, RotateCW, RotateCCW]
                              <&> (\a -> [Nothing, Just a])
                              & sequence
                              <&> catMaybes
                              <&> \c -> (c, oacMakeCache ac (if null c then OEmpty else outputString (foldr1 OAlt (fmap OSingle c))))
                      in \c -> fromJust (lookup c m)
