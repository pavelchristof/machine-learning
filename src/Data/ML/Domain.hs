{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Data.ML.Domain
Description :  Total maps.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

A domain in the sense of this module is actually a total map.
-}
module Data.ML.Domain where

import           Data.Bytes.Serial
import           Data.Key
import           Data.ML.Internal.Domain
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Linear
import           Prelude hiding (zip, lookup)

-- | Domain.
class ( Zip a
      , Indexable a
      , Adjustable a
      , Applicative a
      , Additive a
      , Serial1 a
      ) => Domain a

-- | Finite domain.
class ( Domain a
      , Foldable a
      , Metric a
      ) => FiniteDomain a

-- | Dense domain.
class ( FiniteDomain a
      , ZipWithKey a
      , TraversableWithKey a
      ) => DenseDomain a

-- | Map for a dense ordered domain implemented with a total map.
newtype OrdDomain k a = OrdDomain (Map k a)

type instance Key (OrdDomain k) = k

deriving instance (Show k, Show a) => Show (OrdDomain k a)
deriving instance Functor (OrdDomain k)
deriving instance Keyed (OrdDomain k)
deriving instance Ord k => Lookup (OrdDomain k)
deriving instance Ord k => Indexable (OrdDomain k)
deriving instance Ord k => Adjustable (OrdDomain k)
deriving instance Ord k => Zip (OrdDomain k)
deriving instance Ord k => ZipWithKey (OrdDomain k)
deriving instance Foldable (OrdDomain k)
deriving instance FoldableWithKey (OrdDomain k)

instance (Ord k, Serial k) => Serial1 (OrdDomain k) where
    serializeWith f (OrdDomain m) =
        serializeWith f m
    deserializeWith f = OrdDomain
        <$> deserializeWith f

instance (Ord k, Enum k, Bounded k) => Additive (OrdDomain k) where
    zero = pure 0

instance (Ord k, Enum k, Bounded k) => Metric (OrdDomain k)

instance Traversable (OrdDomain k) where
    traverse f (OrdDomain m) = fmap OrdDomain (traverse f m)

instance TraversableWithKey (OrdDomain k) where
    traverseWithKey f (OrdDomain m) = fmap OrdDomain (traverseWithKey f m)

instance (Ord k, Enum k, Bounded k) => Applicative (OrdDomain k) where
    pure x = OrdDomain $ Map.fromList [(k, x) | k <- [minBound .. maxBound]]
    (<*>) = zap

instance (Ord k, Enum k, Bounded k, Serial k) => Domain (OrdDomain k)
instance (Ord k, Enum k, Bounded k, Serial k) => FiniteDomain (OrdDomain k)
instance (Ord k, Enum k, Bounded k, Serial k) => DenseDomain (OrdDomain k)

-- | Map for a sparse ordered domain implemented with a partial map
-- and a fallback default value.
data SparseOrdDomain k a = SparseOrdDomain (Map k a) a

type instance Key (SparseOrdDomain k) = k

deriving instance (Show k, Show a) => Show (SparseOrdDomain k a)
deriving instance Functor (SparseOrdDomain k)

instance Ord k => Lookup (SparseOrdDomain k) where
    lookup k (SparseOrdDomain m d) = case lookup k m of
                                     Nothing -> Just d
                                     x -> x

instance Ord k => Indexable (SparseOrdDomain k) where
    index (SparseOrdDomain m d) k = case lookup k m of
                                    Nothing -> d
                                    Just x -> x

instance Ord k => Adjustable (SparseOrdDomain k) where
    adjust f k (SparseOrdDomain m d) = SparseOrdDomain (Map.alter f' k m) d
      where
        f' (Just x) = Just (f x)
        f' Nothing = Just (f d)
    replace k v (SparseOrdDomain m d) = SparseOrdDomain (replace k v m) d

instance Ord k => Zip (SparseOrdDomain k) where
    zip (SparseOrdDomain m1 d1) (SparseOrdDomain m2 d2) =
      SparseOrdDomain
        (Map.mergeWithKey
          (\_ a b -> Just (a, b))
          (fmap (, d2))
          (fmap (d1, ))
          m1 m2)
        (d1, d2)

instance (Ord k, Enum k, Bounded k) => Foldable (SparseOrdDomain k) where
    foldMap f (SparseOrdDomain m d) = runSparseFold (f d) $ \_ ->
           foldPoint (toInteger (fromEnum (minBound :: k)) - 1) mempty
        <> Map.foldMapWithKey (\k v -> foldPoint (toInteger (fromEnum k)) (f v)) m
        <> foldPoint (toInteger (fromEnum (maxBound :: k)) + 1) mempty

instance Ord k => Applicative (SparseOrdDomain k) where
    pure x = SparseOrdDomain Map.empty x
    (<*>) = zap

instance Ord k => Additive (SparseOrdDomain k) where
    zero = pure 0

instance (Ord k, Enum k, Bounded k) => Metric (SparseOrdDomain k)

instance (Ord k, Serial k) => Serial1 (SparseOrdDomain k) where
    serializeWith f (SparseOrdDomain m d) =
        serializeWith f m *>
        f d
    deserializeWith f = SparseOrdDomain
        <$> deserializeWith f
        <*> f

instance (Ord k, Serial k) => Domain (SparseOrdDomain k)
instance (Ord k, Enum k, Bounded k, Serial k) => FiniteDomain (SparseOrdDomain k)

-- | Converts a sparse domain to a dense domain.
toDense :: Ord k => Enum k => Bounded k
        => SparseOrdDomain k a -> OrdDomain k a
toDense (SparseOrdDomain m d) = OrdDomain (m <> dm)
  where (OrdDomain dm) = pure d
