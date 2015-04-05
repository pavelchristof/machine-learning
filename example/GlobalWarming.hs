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
import           Data.Monoid (All(..), Sum(..))
import           Data.Function
import           Data.Random
import           Data.Reflection

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

type TreeBank = Tree (Const Text)
type Sentiment = Scalar
type WordVec = V 5

type WordLookup s = Index (TotalSubsetArray s Text) WordVec
type Combinator = AffineMap (Product WordVec WordVec) WordVec :>> Over WordVec Tanh

type Fold s = TreeAlgebra (WordLookup s) Combinator

type SentimentModel s
    = Cata TreeBank (Fold s)
  :>> AffineMap WordVec Sentiment
  :>> Over Sentiment Sigmoid

--------------------------------------------------------------------------------
-- Cost function
--------------------------------------------------------------------------------

cost :: Subset s Text => Cost (SentimentModel s)
cost = Cost cost' + 0.1 * l2reg
  where
    cost' x _ = getScalar $ getSum $ foldMap (Sum . j) x
    j (a, e) = let y = a - e in y * y

--------------------------------------------------------------------------------
-- Driver
--------------------------------------------------------------------------------

driver :: Subset s Text
       => DataSet (SentimentModel s) Double
       -> DataSet (SentimentModel s) Double
       -> Proxy s
       -> Driver (SentimentModel s) Double ()
driver trainingSet testSet _ = do
    setCostFun cost
    setTrainingSet trainingSet
    setTestSet testSet

    load "models/global_warming.bm"
    timed (train 30)
    test isOk

    save "models/global_warming.bm"

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

data Class = NA | Yes | No
    deriving (Eq)

type Tweet = (Text, (Class, Double))

tweetParser :: Parser Tweet
tweetParser = do
    t <- quoted
    char ','
    c <- char '"' *> cls <* char '"'
    char ','
    s <- double
    endOfLine
    return (t, (c, s))
  where
    quoted = char '"' *> takeTill (=='"') <* char '"'
    cls =  string "N/A" *> pure NA
       <|> string "Yes" *> pure Yes
       <|> string "No" *> pure No

tweetsParser :: Parser [Tweet]
tweetsParser = do
    tweets <- many tweetParser
    endOfInput
    return tweets

loadTweets :: IO [Tweet]
loadTweets = do
    content <- Text.readFile "data/global_warming.csv"
    let result = parseOnly tweetsParser content
    either die return result

--------------------------------------------------------------------------------
-- Preprocessing
--------------------------------------------------------------------------------

tokenize :: Text -> [Text]
tokenize = Text.words
         . Text.toLower
         . Text.filter (\c -> isAlphaNum c || isSpace c)

isOk :: Sentiment Double -> Sentiment Double -> CountCorrect
isOk x y = if getAll (foldMap All $ ((==) `on` realToClass) <$> x <*> y)
              then correct
              else incorrect

classToReal :: Class -> Double
classToReal NA = 0.5
classToReal Yes = 1
classToReal No = 0

realToClass :: Double -> Class
realToClass x
    | x >= 0 && x < 1/3 = No
    | x >= 1/3 && x < 2/3 = NA
    | x >= 2/3 && x <= 1 = Yes
    | otherwise = error "wrong class"

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Load the data and shuffle it.
    tweets <- loadTweets
    tweets' <- sample $ shuffle tweets

    -- Tokenize the sentences.
    let tweets'' = fmap (first tokenize) tweets'
        wordSet = Set.fromList ("STOP" : join (fmap fst tweets''))

    -- Prepare the data set.
    let dataSet = Vector.fromList
                $ fmap (bimap (listToTree (Const "STOP") . fmap Const)
                              (Scalar . classToReal . fst))
                $ tweets''
        (trainingSet, testSet) = Vector.splitAt 4500 dataSet

    -- Run the driver.
    reify wordSet $ runDriverT . driver trainingSet testSet
