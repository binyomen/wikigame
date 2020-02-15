module NGramModel
    ( NGramModel
    , makeModel
    ) where

import Data.Char (isSpace)
import Data.Map.Strict (Map)

data TextWord =
    TextStart |
    Literal String

data WordMap =
    WordMap (Map TextWord WordMap) |
    Count Word

data NGramModel = NGramModel
    { ngm_n :: Int
    , ngm_data :: WordMap
    }

makeModel :: Int -> String -> NGramModel
makeModel n text =
    NGramModel { ngm_n = n, ngm_data = parse text }

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

constructMap :: [String] -> WordMap
constructMap words =
    Count 0

parse :: String -> WordMap
parse =
    constructMap . tokenize
