{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
module Data.ML.Model where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Linear

class (Traversable m, Additive m) => Model (m :: * -> *) where
    type Input m :: * -> *
    type Output m :: * -> *
    predict :: Floating a => Input m a -> m a -> Output m a

data AModel a b where
    AModel :: Model m => (forall a. m a) -> AModel (Input m) (Output m)

-- | A composition of models.
data CompositeModel f g a = CompositeModel (f a) (g a)

instance (Functor f, Functor g) => Functor (CompositeModel f g) where
    fmap h (CompositeModel f g) = CompositeModel (fmap h f) (fmap h g)

instance (Additive f, Additive g) => Additive (CompositeModel f g) where
    zero = CompositeModel zero zero
    CompositeModel f g ^+^ CompositeModel f' g' =
        CompositeModel (f ^+^ f') (g ^+^ g')
    CompositeModel f g ^-^ CompositeModel f' g' =
        CompositeModel (f ^-^ f') (g ^-^ g')
    lerp alpha (CompositeModel f g) (CompositeModel f' g') =
        CompositeModel (lerp alpha f f') (lerp alpha g g')
    liftU2 h (CompositeModel f g) (CompositeModel f' g') =
        CompositeModel (liftU2 h f f') (liftU2 h g g')
    liftI2 h (CompositeModel f g) (CompositeModel f' g') =
        CompositeModel (liftI2 h f f') (liftI2 h g g')

instance (Foldable f, Foldable g) => Foldable (CompositeModel f g) where
    foldMap h (CompositeModel f g) = foldMap h f <> foldMap h g

instance (Traversable f, Traversable g) => Traversable (CompositeModel f g) where
    traverse h (CompositeModel f g) = CompositeModel
        <$> traverse h f
        <*> traverse h g

instance (Model f, Model g, Output f ~ Input g) => Model (CompositeModel f g) where
    type Input (CompositeModel f g) = Input f
    type Output (CompositeModel f g) = Output g
    predict x (CompositeModel f g) = predict (predict x f) g

-- | Chains two models.
(|>) :: AModel a b -> AModel b c -> AModel a c
AModel a |> AModel b = AModel (CompositeModel a b)
