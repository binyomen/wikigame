module NGramModel
    ( NGramModel
    , makeModel
    ) where

import Data.Map.Strict (Map)

data WordMap =
    WordMap (Map String WordMap) |
    Word String

data NGramModel = NGramModel
    { n :: Int
    , dat :: WordMap
    }

makeModel :: Int -> String -> NGramModel
makeModel n text =
    NGramModel { n = n, dat = parseText text }

parseText :: String -> WordMap
parseText text =
    Word "test"
