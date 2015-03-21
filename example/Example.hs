{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
import Data.ML
import Data.Foldable
import Control.Applicative
import Numeric.AD hiding (Scalar)
import Data.Random
import Prelude hiding (sum, mapM_)

newtype Letter = Letter Char
    deriving (Eq, Ord, Enum)

instance Bounded Letter where
    minBound = Letter 'a'
    maxBound = Letter 'z'

type ExampleModel
    = MonoidHomModel (OrdDomain Letter) (Matrix' 10)
  :>> LinearModel (Matrix' 10) (Matrix' 5)
  :>> Over (Matrix' 5) Sigmoid
  :>> LinearModel (Matrix' 5) Scalar
  :>> Sigmoid

predict' :: Floating a => String -> ExampleModel a -> a
predict' word = getScalar . predict (toFreeMonoid (fmap Letter word))

dataset :: Floating a => [(String, a)]
dataset = [ ("a", 1)
          , ("aa", 0)
          , ("aaa", 1)
          , ("aaaa", 0)
          ]

lambda :: Floating a => a
lambda = 0.01

errAt :: Floating a => ExampleModel a -> (String, a) -> a
errAt model (input, expected) = (1 - expected) * actual - expected * actual
  where actual = log (predict' input model)

err :: Floating a => ExampleModel a -> a
err model = lambda * quadrance model
          + sum (map (errAt model) dataset)

untilConvergence :: (Epsilon a, RealFloat a, Floating a) => [a] -> [a]
untilConvergence [] = []
untilConvergence (x:y:xs) =
    if isNaN x || nearZero (x - y)
       then []
       else x : untilConvergence (y : xs)
untilConvergence (x:xs) =
    if isNaN x
       then []
       else x : untilConvergence xs

main :: IO ()
main = do
    model <- sample (generate (normal (0 :: Double) 0.01))
    let iters = gradientDescent err model
        errs = untilConvergence (map err iters)
    mapM_ print errs
