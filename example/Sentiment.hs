{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
import           Data.ML
import           Data.Attoparsec.Text
import           Data.Text (Text)
import           Data.Total.Array.Subset
import           System.Exit
import           Data.Foldable
import           Control.Monad
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Typeable
import qualified Data.Text.IO as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Bifunctor
import           Data.Char
import           Data.Random
import           Data.Reflection

type WordVec = V 10
type TreeBank = Tree (Const Text)
type Sentiments = V 2

type Fold s = TreeAlgebra
    (Index (TotalSubsetArray s Text) WordVec)
    (AffineMap (Product WordVec WordVec) WordVec :>> Over WordVec Tanh)

type SentimentModel s
    = Cata TreeBank (Fold s)
  :>> AffineMap WordVec Sentiments
  :>> Softmax Sentiments

cost :: Subset s Text => Cost (SentimentModel s)
cost = logistic + 0.01 * l2reg

tweetParser :: Parser (Text, Int)
tweetParser = do
    takeTill (==',')
    char ','
    s <- decimal
    char ','
    takeTill (==',')
    char ','
    t <- takeTill isEndOfLine
    endOfLine
    return (t, s)

tweetsParser :: Parser (Vector (Text, Int))
tweetsParser = do
    tweets <- many tweetParser
    endOfInput
    return (Vector.fromList tweets)

loadTweets :: IO (Vector (Text, Int))
loadTweets = do
    content <- Text.readFile "data/tweets.csv"
    let result = parseOnly tweetsParser content
    either die return result

tokenize :: Text -> [Text]
tokenize = Text.words
         . Text.toLower
         . Text.filter (\c -> isAlphaNum c || isSpace c)

makeTree :: [Text] -> Fix1 TreeBank a
makeTree [x] = Fix1 $ Leaf (Const x)
makeTree (x:xs) = Fix1 $ Node (makeTree [x]) (makeTree xs)
makeTree _ = error "Empty sentence!"

makeSentiment :: Int -> Sentiments Double
makeSentiment 0 = V [1, 0]
makeSentiment 1 = V [0, 1]
makeSentiment _ = error "Invalid sentiment."

isOk :: Sentiments Double -> Sentiments Double -> CountCorrect
isOk x y = if all id (ok <$> x <*> y)
              then correct
              else incorrect
  where ok a b = abs (a - b) < 0.5

main :: IO ()
main = do
    tweets <- loadTweets
    let tweets' = Vector.take 10000 tweets
        sentences = fmap (first tokenize) tweets'
        wordSet = Set.fromList $ join $ toList $ fmap fst sentences
        dataSet = fmap (bimap makeTree makeSentiment) sentences
        (trainingSet, testSet) = Vector.splitAt 1000 dataSet

        driver :: Subset s Text => Proxy s -> Driver (SentimentModel s) Double ()
        driver _ = do
            setCostFun cost
            setTrainingSet trainingSet
            setTestSet testSet

            genModel (uniform (-0.1) 0.1)
            timed (train 10)

    reify wordSet $ \p -> runDriverT (driver p)
