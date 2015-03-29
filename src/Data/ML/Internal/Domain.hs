{-# LANGUAGE RankNTypes #-}
module Data.ML.Internal.Domain where

import Data.Proxy
import Data.Reflection
import Data.Semigroup

-- | `mpower x n` raises x to the power n taking advantage of associativity.
mpower :: Monoid m => m -> Integer -> m
mpower _ 0 = mempty
mpower x 1 = x
mpower x n = mpower x r `mappend` mpower (x `mappend` x) q
  where (q, r) = quotRem n 2

-- | A semigroup used to fold a sparse finite domain.
data SparseFold s m = SparseFold (Min Integer) m (Max Integer)

instance (Reifies s m, Monoid m) => Semigroup (SparseFold s m) where
    SparseFold min x max <> SparseFold min' y max' =
        SparseFold
          (min <> min')
          (x `mappend` mpower filler gapSize `mappend` y)
          (max <> max')
      where
        gapSize = getMin min' - getMax max - 1
        filler = reflect (Proxy :: Proxy s)

foldPoint :: Integer -> a -> Option (SparseFold s a)
foldPoint k v = Option $ Just $ SparseFold (Min k) v (Max k)

runSparseFold :: Monoid m => m -> (forall s. Reifies s m => Proxy s -> Option (SparseFold s m)) -> m
runSparseFold d f = reify d $ \p -> extract (f p)
  where extract (Option Nothing) = mempty
        extract (Option (Just (SparseFold _ v _))) = v
