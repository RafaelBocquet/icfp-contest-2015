module DNA where

import Prelude ()
import MyPrelude

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import qualified KMP as KMP
import System.IO
import Control.DeepSeq
import Control.DeepSeq.Generics

data Base = I | C | F | P
          deriving (Eq, Ord, Show, Generic)
instance NFData Base where
  rnf = genericRnf
type DNA = Seq Base

type Parser a = ExceptT () (StateT (DNA, DNA) (StateT Int (WriterT [[Base]] IO))) a

readBase :: Char -> Base
readBase 'I' = I
readBase 'C' = C
readBase 'F' = F
readBase 'P' = P
readBase _ = error "readBase"

next :: Parser Base
next = do
  (as, bs) <- get
  case Seq.viewl bs of
    b Seq.:< bs -> do put (as :> b, bs); pure b
    Seq.EmptyL   -> throwError ()

skip :: Integer -> Parser DNA
skip n = do
  (as, bs) <- get
  when (fromIntegral (Seq.length bs) < n) (throwError ())
  let (cs, ds) = Seq.splitAt (fromIntegral n) bs
  put (as <> cs, ds)
  pure cs

data PItem = PBase !Base | PNat !Integer | PDNA !DNA | PPat !Pattern
           deriving (Show)
type Pattern = [PItem]

nats :: Parser Integer
nats = do
  next >>= \case
    P -> pure 0
    C -> (+ 1) . (* 2) <$> nats
    _ -> (* 2) <$> nats

consts :: Parser DNA
consts = do
  a <- get
  (`catchError` (const (do put a; pure Empty))) $
    next >>= \case
      C -> (I :<) <$> consts
      F -> (C :<) <$> consts
      P -> (F :<) <$> consts
      I -> next >>= \case
        C -> (P :<) <$> consts
        _ -> throwError ()

pattern_ :: Parser Pattern
pattern_ = do
  next >>= \case
    C -> (PBase I :<) <$> pattern_
    F -> (PBase C :<) <$> pattern_
    P -> (PBase F :<) <$> pattern_
    I -> next >>= \case
      C -> (PBase P :<) <$> pattern_
      P -> (:<) <$> (PNat <$> nats) <*> pattern_
      F -> do next; (:<) <$> (PDNA <$> consts) <*> pattern_
      I -> next >>= \case
        I -> do skip 7 >>= tell . (:[]) . toList; pattern_
        P -> (:<) <$> (PPat <$> pattern_) <*> pattern_
        _ -> pure Empty

data TItem = TBase Base | TLen !Integer | TQuote !Integer !Integer
           deriving (Show)
type Template = [TItem]

template :: Parser Template
template = do
  next >>= \case
    C -> (TBase I :<) <$> template
    F -> (TBase C :<) <$> template
    P -> (TBase F :<) <$> template
    I -> next >>= \case
      C -> (TBase P :<) <$> template
      I -> next >>= \case
        P -> (:<) <$> (TLen <$> nats) <*> template
        I -> do skip 7 >>= tell . (:[]) . toList; template
        _ -> pure Empty
      _ -> (:<) <$> (TQuote <$> nats <*> nats) <*> template

match :: PItem -> Parser (Seq DNA)
match (PBase b) = next <&> (== b) >>= bool (throwError ()) (pure Empty)
match (PNat n)  = do skip n; pure Empty
match (PDNA d)  = do
  let t = KMP.build (toList d)
  ss <- use _2
  case KMP.match t ss of
    Nothing -> throwError ()
    Just (as, bs) -> do _1 %= (<> as); _2 .= bs; pure Empty
match (PPat p)  = do
  a <- use _1
  _1 .= Empty
  r <- mconcat <$> mapM match p
  b <- _1 <<%= (a <>)
  pure (r :> b)

replace :: Template -> Seq DNA -> DNA
replace t e = mconcat $ t <&> \case
  TBase b    -> Seq.singleton b
  TLen n     -> asnat (Seq.length (fold (lookup (fromIntegral n) e)))
  TQuote l n -> protect l (fold (lookup (fromIntegral n) e))

asnat :: Int -> DNA
asnat 0 = Seq.singleton P
asnat n | even n    = I :< asnat (n `div` 2)
        | otherwise = C :< asnat (n `div` 2)

protect :: Integer -> DNA -> DNA
protect 0 a = a
protect l a = mconcat (protectBase l <$> toList a)

protectBase :: Integer -> Base -> DNA
protectBase 0 a = Seq.singleton a
protectBase l I = protectBase (l-1) C
protectBase l C = protectBase (l-1) F
protectBase l F = protectBase (l-1) P
protectBase l P = protectBase (l-1) I <> protectBase (l-1) C

execute :: Parser ()
execute =
  whileM_ ((`catchError` (const (pure False))) $ do
               i <- lift . lift $ get
               when (i`mod`100 == 0) $ do
                 l <- use (_2.to length)
                 liftIO $ hPutStr stderr $ "\r" ++ show i ++ " " ++ show l
                 when (i`mod`10000 == 0) $ liftIO $ hFlush stderr
               p <- pattern_
               t <- template
               a <- use _2
               (`catchError` (const (_2 .= a))) $ do
                 _1 .= Empty
                 e <- mconcat <$> mapM match p
                 _2 %= (DNA.replace t e <>)
               lift . lift $ id += 1
               pure True
          ) (pure ())

runParser :: [Base] -> Parser a -> IO [[Base]]
runParser s = execWriterT . (execStateT ?? 0) . (execStateT ?? (Empty, Seq.fromList s)) . runExceptT
