{- |
Module      :  Data.Vector.Parallel
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

Parallel vector operations.
-}
module Data.Vector.Parallel where

import           Control.Parallel.Strategies
import           Data.Foldable
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           GHC.Conc

-- | Boxed vectors with parallel operations.
newtype ParVector a = ParVector { runParVector :: Vector a }

instance Functor ParVector where
    fmap f (ParVector v) =
        ParVector (fmap f v `using` parTraversable rseq)

instance Applicative ParVector where
    pure x = ParVector (pure x)
    ParVector f <*> ParVector x =
        ParVector (f <*> x `using` parTraversable rseq)

instance Foldable ParVector where
    foldMap f = fold . parMap rseq (foldMap f) . chunk

-- | Splits a vector into a sensible number of parts.
chunk :: ParVector a -> [Vector a]
chunk (ParVector v0) = go v0
  where
    go v | Vector.length v <= n = [v]
         | otherwise = let (l, r) = Vector.splitAt n v
                       in l : go r
    n = (Vector.length v0 `div` (numCapabilities * 4)) + 1
