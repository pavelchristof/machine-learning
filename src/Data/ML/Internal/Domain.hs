{-# LANGUAGE RankNTypes #-}
module Data.ML.Internal.Domain where

import Data.Semigroup
import Data.Reflection

-- | `mpower x n` raises x to the power n taking advantage of associativity.
mpower :: Monoid m => m -> Integer -> m
mpower _ 0 = mempty
mpower x 1 = x
mpower x n = mpower x r `mappend` mpower (x `mappend` x) q
  where (q, r) = quotRem n 2

-- | A semigroup used to fold a sparse enum domain.
data SparseFold m = SparseFold (Min Integer) m (Max Integer)

instance (Given m, Monoid m) => Semigroup (SparseFold m) where
    SparseFold min x max <> SparseFold min' y max' =
        SparseFold
          (min <> min')
          (x `mappend` mpower given gapSize `mappend` y)
          (max <> max')
      where
        gapSize = getMin min' - getMax max - 1

foldPoint :: Integer -> a -> Option (SparseFold a)
foldPoint k v = Option $ Just $ SparseFold (Min k) v (Max k)

runSparseFold :: Monoid m => m -> (Given m => Option (SparseFold m)) -> m
runSparseFold d f = give d $ extract f
  where extract (Option Nothing) = mempty
        extract (Option (Just (SparseFold _ v _))) = v
