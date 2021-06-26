{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#ifdef TEST
module NGramModel where
#else
module NGramModel
    ( makeModel
    , NGramModel
    , scoreText
    ) where
#endif

import MemSize (MemSize, memSize)

import Data.Hashable (Hashable, hashWithSalt)
import Data.HashMap.Strict (HashMap); import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet); import qualified Data.HashSet as S
import Data.Text (Text); import qualified Data.Text as T

data TextWord =
    TextStart |
    Literal Text

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

instance Hashable TextWord where
    hashWithSalt salt TextStart = hashWithSalt salt ("<TextStart>" :: Text)
    hashWithSalt salt (Literal t) = hashWithSalt salt t

instance Show TextWord where
    show tw =
        case tw of
            TextStart -> "<TextStart>"
            Literal s -> T.unpack s

instance MemSize TextWord where
    memSize TextStart = 0
    memSize (Literal s) = memSize s

data WordMap =
    WordMap (HashMap TextWord WordMap) |
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

instance MemSize WordMap where
    memSize (WordMap m) = memSize m
    memSize (Count c) = memSize c

data NGramModel = NGramModel
    { ngm_n :: Word
    , ngm_smoothed :: Bool
    , ngm_data :: WordMap
    }

instance MemSize NGramModel where
    memSize NGramModel{ngm_n = n, ngm_smoothed = smoothed, ngm_data = dat} = memSize n + memSize smoothed + memSize dat

-- taken from the NLTK: https://gist.github.com/sebleier/554280
stopWords :: HashSet Text
stopWords = S.fromList
    [ "i"
    , "me"
    , "my"
    , "myself"
    , "we"
    , "our"
    , "ours"
    , "ourselves"
    , "you"
    , "your"
    , "yours"
    , "yourself"
    , "yourselves"
    , "he"
    , "him"
    , "his"
    , "himself"
    , "she"
    , "her"
    , "hers"
    , "herself"
    , "it"
    , "its"
    , "itself"
    , "they"
    , "them"
    , "their"
    , "theirs"
    , "themselves"
    , "what"
    , "which"
    , "who"
    , "whom"
    , "this"
    , "that"
    , "these"
    , "those"
    , "am"
    , "is"
    , "are"
    , "was"
    , "were"
    , "be"
    , "been"
    , "being"
    , "have"
    , "has"
    , "had"
    , "having"
    , "do"
    , "does"
    , "did"
    , "doing"
    , "a"
    , "an"
    , "the"
    , "and"
    , "but"
    , "if"
    , "or"
    , "because"
    , "as"
    , "until"
    , "while"
    , "of"
    , "at"
    , "by"
    , "for"
    , "with"
    , "about"
    , "against"
    , "between"
    , "into"
    , "through"
    , "during"
    , "before"
    , "after"
    , "above"
    , "below"
    , "to"
    , "from"
    , "up"
    , "down"
    , "in"
    , "out"
    , "on"
    , "off"
    , "over"
    , "under"
    , "again"
    , "further"
    , "then"
    , "once"
    , "here"
    , "there"
    , "when"
    , "where"
    , "why"
    , "how"
    , "all"
    , "any"
    , "both"
    , "each"
    , "few"
    , "more"
    , "most"
    , "other"
    , "some"
    , "such"
    , "no"
    , "nor"
    , "not"
    , "only"
    , "own"
    , "same"
    , "so"
    , "than"
    , "too"
    , "very"
    , "s"
    , "t"
    , "can"
    , "will"
    , "just"
    , "don"
    , "should"
    , "now"
    ]

makeModel :: Word -> Bool -> Text -> NGramModel
makeModel n smoothed text =
    NGramModel{ngm_n = n, ngm_smoothed = smoothed, ngm_data = parse (n - 1) text}

constructMap :: Word -> [Text] -> WordMap
constructMap numPreceding =
    constructMapRecurse $ replicate (fromIntegral numPreceding) TextStart

constructMapRecurse :: [TextWord] -> [Text] -> WordMap
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

tokenize :: Text -> [Text]
tokenize text = filter (not . (`S.member` stopWords)) $ T.words text

parse :: Word -> Text -> WordMap
parse numPreceding =
    constructMap numPreceding . tokenize

scoreText :: NGramModel -> Text -> Double
scoreText model text =
    sum wordScores
    where
        tokens = tokenize text
        numPreceding = ngm_n model - 1
        initialPreceding = replicate (fromIntegral numPreceding) TextStart
        wordScores = scoreWords model tokens initialPreceding

scoreWords :: NGramModel -> [Text] -> [TextWord] -> [Double]
scoreWords model (first : rest) preceding =
    scoreWord model first preceding :
        scoreWords model rest nextPreceding
    where
        nextPreceding = tail $ preceding ++ [Literal first]
scoreWords _ [] _ = []

scoreWord :: NGramModel -> Text -> [TextWord] -> Double
scoreWord model word preceding =
    log probability
    where
        numerator :: Double
        numerator = fromIntegral $ getCount (ngm_data model) (preceding ++ [Literal word])
        smoothedNumerator :: Double
        smoothedNumerator =
            if ngm_smoothed model && numerator == 0 then
                1
            else
                numerator
        denominator :: Double
        denominator = fromIntegral $ totalCount $ ngm_data model
        probability :: Double
        probability = smoothedNumerator / denominator

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
