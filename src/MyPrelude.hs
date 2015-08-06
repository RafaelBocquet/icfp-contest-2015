module MyPrelude
       ( module Prelude
       , module Data.List
       , module Data.Maybe
       , module Data.Bool
       , module Data.Char
       , module Data.Monoid
       , module Data.Functor
       , module Data.Foldable
       , module Data.Traversable
       , module Data.Bifunctor
       , module Data.Bifoldable
       , module Data.Bitraversable
       , module Control.Lens
       , module Control.Applicative
       , module Control.Monad
       , module Control.Monad.Reader
       , module Control.Monad.State
       , module Control.Monad.Except
       , module Control.Monad.Writer
       , module Control.Monad.Cont
       , module Control.Monad.ST
       , module Control.Monad.Loops
       , module GHC.Generics
       , module Data.Key
       , module Data.Either.Located
       , module Data.List.Located
       , module Data.Maybe.Located
       , module GHC.Err.Located
       , module Control.Applicative.Unicode
       , module Control.Arrow.Unicode
       , module Control.Category.Unicode
       , module Control.Monad.Unicode
       , module Data.Bool.Unicode
       , module Data.Eq.Unicode
       , module Data.Foldable.Unicode
       , module Data.List.Unicode
       , module Data.Monoid.Unicode
       , module Data.Ord.Unicode
       , module Prelude.Unicode
       , module Control.DeepSeq
       , module Control.DeepSeq.Generics

       , module System.IO

       , Seq, Map, Set, IntMap, IntSet, Vector, STVector, IOVector

       , pattern Empty
       , pattern (:>)
       , pattern (:<)
       )
       where

import Prelude hiding (error, undefined, lookup, zipWith, zip, (!!), cycle, head, init, tail, last)
import Data.List hiding (uncons, lookup, zipWith, zip, (!!), cycle, foldl1, head, init, last, tail, foldl1', foldr1, maximum, minimum)
import Data.Maybe hiding (fromJust)
import Data.Bool
import Data.Char
import Data.Monoid
import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Control.Lens hiding (Indexable, index)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Monad.ST
import Control.Monad.Loops
import GHC.Generics (Generic)

import Data.Key

import Data.Either.Located
import Data.List.Located hiding (uncons, lookup, zipWith, zip, foldl1, foldr1, minimum, maximum)
import Data.Maybe.Located
import GHC.Err.Located

import Control.Applicative.Unicode hiding ((∅))
import Control.Arrow.Unicode
import Control.Category.Unicode
import Control.Monad.Unicode
import Data.Bool.Unicode
import Data.Eq.Unicode
import Data.Foldable.Unicode
import Data.List.Unicode hiding ((∈), (∋), (∉), (∌))
import Data.Monoid.Unicode
import Data.Ord.Unicode
import Prelude.Unicode hiding ((∘), (∈), (∉))

import Control.DeepSeq
import Control.DeepSeq.Generics
import System.IO

import Data.Sequence (Seq)
import Data.Map (Map)
import Data.Set (Set)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Vector (Vector)
import Data.Vector.Mutable (STVector, IOVector)

pattern Empty <- (has _Empty -> True) where
  Empty = review _Empty ()

infixr 5 :<
infixl 5 :>

pattern (:<) a s <- (preview _Cons -> Just (a,s)) where
  (:<) a s = _Cons # (a,s)

pattern (:>) s a <- (preview _Snoc -> Just (s,a)) where
  (:>) a s = _Snoc # (a,s)
