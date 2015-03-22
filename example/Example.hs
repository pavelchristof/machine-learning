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

dataset :: Floating a => [(FreeMonoid Letter a, Scalar a)]
dataset = map (bimap (toFreeMonoid . map Letter) Scalar)
        $ map (\s -> (s, if evenCount 'a' s && evenCount 'b' s
                            then 1
                            else 0))
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

main2 :: IO ()
main2 = do
    let s = ["3.2910313625311662",
            "-3.720233656325635",
            "1.7e-322",
            "-1.2138356404396925e-6",
            "-3.323441165146289e-5",
            "-3.937484289013896e-6",
            "-2.7903723900230495e-13",
            "-9.929445010038518e-32",
            "-8.615456026547715e-24",
            "1.7268739964134595e-26",
            "-8.725096901710127e-3",
            "7.638015308304665e-14",
            "-5.099212878516502e-42",
            "5.8216023355334485e-5",
            "-3.5942599108900903e-7",
            "-1.555149636884462e-27",
            "-3.96299773824206e-6",
            "-4.967146520776881e-19",
            "-1.2023355746871941e-14",
            "-9.226291844483162e-4",
            "2.1642570295673594e-40",
            "-1.1101779414799364e-2",
            "-1.3279381813507682e-20",
            "6.0898687604799085e-192",
            "1.3089885372040525e-4",
            "-6.019190858680441e-11"]
    let cs :: [Double]
        cs = map read s
    let model :: ExampleModel Double
        model = evalState (traverse (\_ -> state (\(x:xs) -> (x, xs))) (pure ())) cs
    forM_ dataset $ \(i, o) -> do
         print ((\(FreeMonoid m) -> m) i (\(Letter c) -> c:[]))
         print (getScalar $ predict i model)
         print (getCost cost (Identity (predict i model, o)) model)

main :: IO ()
main = do
    model <- sample $ generate (normal 0 (1 :: Double))
    batches <- sample $ replicateM 10000 $ randomElement (map Identity dataset)
    let iters = adaGrad batches cost model
    mapM_ print (map fst iters)
    runInputT defaultSettings $ repl (snd (last iters))
