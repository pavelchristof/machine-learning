{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{- |
Module      :  Data.ML.Model
Description :  Model class.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  pawel834@gmail.com
Stability   :  experimental

-}
module Data.ML.Model where

import Control.Applicative
import Data.Bytes.Serial
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Linear

-- | A machine learning model.
class Model m where
    type Input m :: * -> *
    type Output m :: * -> *
    predict :: Floating a => Input m a -> m a -> Output m a

-- | Generates a model with an applicative.
generate :: (Applicative f, Applicative g, Traversable g) => f a -> f (g a)
generate f = traverse (const f) (pure ())

-- | A model that passes through its input.
newtype IdentityModel (f :: * -> *) (a :: *) = IdentityModel (V0 a)
     deriving (Functor, Applicative, Monad, Foldable, Traversable, Additive, Metric)

instance Serial1 (IdentityModel f) where
    serializeWith _ _ = return ()
    deserializeWith _ = return (IdentityModel V0)

instance Model (IdentityModel f) where
    type Input (IdentityModel f) = f
    type Output (IdentityModel f) = f
    predict x _ = x

-- | A composition of models.
data (f :>> g) a = (f a) :>> (g a)

instance (Functor f, Functor g) => Functor (f :>> g) where
    fmap h (f :>> g) = fmap h f :>> fmap h g

instance (Applicative f, Applicative g) => Applicative (f :>> g) where
    pure x = pure x :>> pure x
    (f :>> f') <*> (x :>> x') = (f <*> x) :>> (f' <*> x')

instance (Additive f, Additive g) => Additive (f :>> g) where
    zero = zero :>> zero
    (f :>> g) ^+^ (f' :>> g') =
        (f ^+^ f') :>> (g ^+^ g')
    (f :>> g) ^-^ (f' :>> g') =
        (f ^-^ f') :>> (g ^-^ g')
    lerp alpha (f :>> g) (f' :>> g') =
        lerp alpha f f' :>> lerp alpha g g'
    liftU2 h (f :>> g) (f' :>> g') =
        liftU2 h f f' :>> liftU2 h g g'
    liftI2 h (f :>> g) (f' :>> g') =
        liftI2 h f f' :>> liftI2 h g g'

instance (Metric f, Metric g) => Metric (f :>> g) where
    dot (f :>> g) (f' :>> g') = dot f f' + dot g g'
    quadrance (f :>> g) = quadrance f + quadrance g
    qd (f :>> g) (f' :>> g') = qd f f' + qd g g'

instance (Foldable f, Foldable g) => Foldable (f :>> g) where
    foldMap h (f :>> g) = foldMap h f <> foldMap h g

instance (Traversable f, Traversable g) => Traversable (f :>> g) where
    traverse h (f :>> g) = (:>>)
        <$> traverse h f
        <*> traverse h g

instance (Serial1 f, Serial1 g) => Serial1 (f :>> g) where
    serializeWith h (f :>> g) =
        serializeWith h f *>
        serializeWith h g
    deserializeWith h = (:>>)
        <$> deserializeWith h
        <*> deserializeWith h

instance (Model f, Model g, Output f ~ Input g) => Model (f :>> g) where
    type Input (f :>> g) = Input f
    type Output (f :>> g) = Output g
    predict x (f :>> g) = predict (predict x f) g
