{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.Foldable hiding (mapM_, forM_)
import Data.Functor.Identity
import Data.List
import Data.ML
import Data.Random
import Data.Traversable
import System.Console.Haskeline

newtype Letter = Letter Char
    deriving (Eq, Ord, Enum)

instance Bounded Letter where
    minBound = Letter 'a'
    maxBound = Letter 'z'

type ExampleModel
    = MonoidHom (OrdDomain Letter) (Matrix' 5)
  :>> AffineMap (Matrix' 5) Scalar
  :>> Sigmoid

cost :: Cost ExampleModel
cost = logistic + 0.01 * l2reg

evenCount :: Char -> [Char] -> Bool
evenCount x xs = length (elemIndices x xs) `rem` 2 == 0

isAccepted :: String -> Bool
isAccepted s = evenCount 'a' s && evenCount 'b' s



dataset :: Floating a => [(FreeMonoid Letter a, Scalar a)]
dataset = map (bimap (toFreeMonoid . map Letter) Scalar)
        $ map (\s -> (s, if isAccepted s then 1 else 0))
        $ [1 .. 5] >>= flip replicateM ['a', 'b']

repl :: Floating a => Show a => ExampleModel a -> InputT IO ()
repl model = do
    input <- getInputLine "word> "
    case input of
      Nothing -> return ()
      Just line -> do
        let result = getScalar $ predict (toFreeMonoid $ map Letter line) model
        outputStrLn (show result)
        repl model

check :: ExampleModel Double -> Int
check model = length $ map (\(i, o) -> abs (getScalar (predict i model) - getScalar o) <= 0.5) dataset

main :: IO ()
main = do
    model <- sample $ generate (normal 0 (1 :: Double))
    --batches <- sample $ replicateM 10000 $ randomElement (map Identity dataset)
    let batches = replicate 5000 dataset
    let iters = adaGrad batches cost model
    mapM_ print (map fst iters)
    let model' = snd (last iters)
        correct = check model'
        total = length dataset
    putStrLn $ show correct ++ "/" ++ show total ++ " correct"
    runInputT defaultSettings $ repl model'
