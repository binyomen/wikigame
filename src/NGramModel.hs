module NGramModel
    ( NGramModel
    , makeModel
    ) where

import Data.Map.Strict (Map)

data WordMap =
    WordMap (Map String WordMap) |
    Word String

data NGramModel = NGramModel
    { ngm_n :: Int
    , ngm_data :: WordMap
    }

makeModel :: Int -> String -> NGramModel
makeModel n text =
    NGramModel { ngm_n = n, ngm_data = parseText text }

parseText :: String -> WordMap
parseText text =
    Word "test"
