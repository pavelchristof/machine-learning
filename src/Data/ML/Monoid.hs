{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Data.ML.Monoid
Description :  Monoid homomorphism models.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

All models in this category are monoid homomorphisms from
the free monoid over the given domain to some model dependent monoid.
-}
module Data.ML.Monoid where

import Data.Monoid

-- | A free monoid over a, with an extra unused type parameter to match
-- the kind of model input.
newtype FreeMonoid a b = FreeMonoid (forall m. Monoid m => (a -> m) -> m)
