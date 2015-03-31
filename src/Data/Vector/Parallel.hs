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
import           Data.Monoid
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

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
    foldMap f (ParVector v)
        | s < 64 = foldMap f v `using` rseq
        | otherwise = runEval $ do
            m1 <- rpar $ foldMap f (ParVector l)
            m2 <- rpar $ foldMap f (ParVector m)
            m3 <- rseq $ foldMap f (ParVector r)
            return (m1 <> m2 <> m3)
      where
        s = Vector.length v
        s' = Vector.length v'
        (l, v') = Vector.splitAt (s `div` 3) v
        (m, r) = Vector.splitAt (s' `div` 2) v'
