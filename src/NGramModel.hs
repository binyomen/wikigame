{-# LANGUAGE CPP #-}

#ifdef TEST
module NGramModel where
#else
module NGramModel
    ( NGramModel
    , makeModel
    ) where
#endif

import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (lookup, insert, empty, singleton)

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

data WordMap =
    WordMap (Map TextWord WordMap) |
    Count Word

data NGramModel = NGramModel
    { ngm_n :: Int
    , ngm_data :: WordMap
    }

makeModel :: Int -> String -> NGramModel
makeModel n text =
    NGramModel { ngm_n = n, ngm_data = parse (n - 1) text }

tokenize :: String -> [String]
tokenize text =
    word : tokenize rest
    where
        (word, rest) = getWord text

getWord :: String -> (String, String)
getWord (first : rest) =
    if isSpace first then
        ("", eatSpaces rest)
    else
        (first : restWord, restWords)
    where
        (restWord, restWords) = getWord rest
getWord [] =
    ("", [])

eatSpaces :: String -> String
eatSpaces (first : rest) =
    if isSpace first then
        eatSpaces rest
    else
        first : rest
eatSpaces [] = ""

constructMap :: Int -> [String] -> WordMap
constructMap numPreceding =
    constructMapRecurse $ replicate numPreceding TextStart

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
        Just m' -> addToMap m' rest
        Nothing -> WordMap $ M.insert first newSubMap m
    where
        newSubMap = addToMap (Count 0) rest
addToMap (WordMap m) [] = WordMap m
addToMap (Count c) (first : rest) =
    WordMap $ M.singleton first subMap
    where
        subMap = addToMap (Count c) rest
addToMap (Count c) [] = Count $ c + 1

parse :: Int -> String -> WordMap
parse numPreceding =
    constructMap numPreceding . tokenize
