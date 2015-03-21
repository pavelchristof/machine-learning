{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Data.ML.Linear where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.ML.Matrix
import Data.ML.Model
import Data.Traversable
import GHC.TypeLits
import Linear
import Prelude hiding (sum)

-- | A linear map.
newtype LinearModel f g a = LinearModel (g (f a))
    deriving (Show, Functor, Foldable, Traversable)

instance (Applicative f, Applicative g) => Applicative (LinearModel f g) where
    pure = LinearModel . pure . pure
    LinearModel f <*> LinearModel x = LinearModel ((<*>) <$> f <*> x)

instance (Additive f, Applicative g) => Additive (LinearModel f g) where
    zero = LinearModel (pure zero)
    liftU2 f (LinearModel m) (LinearModel m') =
        LinearModel (liftA2 (liftU2 f) m m')
    liftI2 f (LinearModel m) (LinearModel m') =
        LinearModel (liftA2 (liftI2 f) m m')

instance (Metric f, Applicative g, Foldable g) => Metric (LinearModel f g) where
    dot (LinearModel m) (LinearModel m') = sum (dot <$> m <*> m')

instance (Serial1 f, Serial1 g) => Serial1 (LinearModel f g) where
    serializeWith f (LinearModel m) = serializeWith (serializeWith f) m
    deserializeWith f = LinearModel <$> deserializeWith (deserializeWith f)

instance (Metric f, Functor g) => Model (LinearModel f g) where
    type Input (LinearModel f g) = f
    type Output (LinearModel f g) = g
    predict f (LinearModel m) = fmap (dot f) m
