{-# LANGUAGE CPP #-}

#ifdef TEST
module NGramModel where
#else
module NGramModel
    ( NGramModel
    , makeModel
    , scoreText
    ) where
#endif

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (lookup, insert, empty, singleton, toList)

data TextWord =
    TextStart |
    Literal String

instance Eq TextWord where
    (==) TextStart (Literal _) = False
    (==) TextStart TextStart = True
    (==) (Literal s) (Literal t) = s == t
    (==) (Literal _) TextStart = False

instance Ord TextWord where
    compare TextStart (Literal _) = LT
    compare TextStart TextStart = EQ
    compare (Literal s) (Literal t) = compare s t
    compare (Literal _) TextStart = GT

    (<=) TextStart (Literal _) = True
    (<=) TextStart TextStart = True
    (<=) (Literal s) (Literal t) = s <= t
    (<=) (Literal _) TextStart = False

instance Show TextWord where
    show tw =
        case tw of
            TextStart -> "<TextStart>"
            Literal s -> s

data WordMap =
    WordMap (Map TextWord WordMap) |
    Count Word

instance Eq WordMap where
    (==) (WordMap m1) (WordMap m2) = m1 == m2
    (==) (WordMap _) (Count _) = False
    (==) (Count _) (WordMap _) = False
    (==) (Count i1) (Count i2) = i1 == i2

instance Show WordMap where
    show wm =
        case wm of
            WordMap m -> show m
            Count i -> show i

data NGramModel = NGramModel
    { ngm_n :: Word
    , ngm_data :: WordMap
    }

makeModel :: Word -> String -> NGramModel
makeModel n text =
    NGramModel { ngm_n = n, ngm_data = parse (n - 1) text }

constructMap :: Word -> [String] -> WordMap
constructMap numPreceding =
    constructMapRecurse $ replicate (fromIntegral numPreceding) TextStart

constructMapRecurse :: [TextWord] -> [String] -> WordMap
constructMapRecurse preceding (first : rest) =
    addToMap restMap wordList
    where
        wordList = preceding ++ [Literal first]
        restMap = constructMapRecurse (tail wordList) rest
constructMapRecurse _preceding [] = WordMap M.empty

addToMap :: WordMap -> [TextWord] -> WordMap
addToMap (WordMap m) (first : rest) =
    case M.lookup first m of
        Just m' -> WordMap $ M.insert first (justSubMap m') m
        Nothing -> WordMap $ M.insert first nothingSubMap m
    where
        justSubMap m' = addToMap m' rest
        nothingSubMap = addToMap (Count 0) rest
addToMap (WordMap m) [] = WordMap m
addToMap (Count c) (first : rest) =
    WordMap $ M.singleton first subMap
    where
        subMap = addToMap (Count c) rest
addToMap (Count c) [] = Count $ c + 1

parse :: Word -> String -> WordMap
parse numPreceding =
    constructMap numPreceding . words

scoreText :: NGramModel -> String -> Double
scoreText model text =
    sum wordScores
    where
        tokens = words text
        numPreceding = ngm_n model - 1
        initialPreceding = replicate (fromIntegral numPreceding) TextStart
        wordScores = scoreWords (ngm_data model) tokens initialPreceding

scoreWords :: WordMap -> [String] -> [TextWord] -> [Double]
scoreWords wordMap (first : rest) preceding =
    scoreWord wordMap first preceding :
        scoreWords wordMap rest nextPreceding
    where
        nextPreceding = tail $ preceding ++ [Literal first]
scoreWords _ [] _ = []

scoreWord :: WordMap -> String -> [TextWord] -> Double
scoreWord wordMap word preceding =
    log probability
    where
        numerator :: Double
        numerator = fromIntegral $ getCount wordMap (preceding ++ [Literal word])
        denominator :: Double
        denominator = fromIntegral $ totalCount wordMap
        probability :: Double
        probability = numerator / denominator

getCount :: WordMap -> [TextWord] -> Word
getCount (WordMap m) (first : rest) =
    case M.lookup first m of
        Just m' -> getCount m' rest
        Nothing -> 0
getCount (WordMap _) [] = 0
getCount (Count _) (_ : _) = 0
getCount (Count c) [] = c

totalCount :: WordMap -> Word
totalCount (WordMap m) =
    sum $ map (\(_, v) -> totalCount v) (M.toList m)
totalCount (Count c) = c