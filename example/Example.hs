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


dataset :: Floating a => [(Const [Letter] a, Scalar a)]
dataset = map (bimap (Const . map Letter) Scalar)
        $ map (\s -> (s, if isAccepted s then 1 else 0))
        $ [0 .. 6] >>= flip replicateM ['a', 'b']

repl :: Floating a => Show a => ExampleModel a -> InputT IO ()
repl model = do
    input <- getInputLine "word> "
    case input of
      Nothing -> return ()
      Just line -> do
        let result = getScalar $ predict (Const $ map Letter line) model
        outputStrLn (show result)
        repl model

check :: ExampleModel Double -> [(Const [Letter] Double, Scalar Double)] -> Int
check model set = length $ map (\(i, o) -> abs (predict i model - o) <= 0.5) set

main :: IO ()
main = do
    -- Prepare data sets.
    dataset' <- sample $ shuffle dataset
    let split = floor $ 0.6 * fromIntegral (length dataset')
        trainingSet = take split dataset'
        validationSet = drop split dataset'

    -- Teach the model.
    model <- sample $ generate (normal 0 (1 :: Double))
    let batches = replicate 200 trainingSet
    let iters = adaGrad batches cost model
    mapM_ print (map fst iters)
    let model' = snd (last iters)

    -- Evaluate the model.
    putStrLn $ show (check model' trainingSet) ++ "/" ++ show (length trainingSet)
             ++ " correct on training set"
    putStrLn $ show (check model' validationSet) ++ "/" ++ show (length validationSet)
             ++ " correct on validation set"
    runInputT defaultSettings $ repl model'
