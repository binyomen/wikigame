module NGramModelSpec (spec) where

import NGramModel

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (empty, singleton, fromList, toList, null)

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    testAddToMap
    testConstructMap

emptyMap :: WordMap
emptyMap = WordMap M.empty

kvpm :: String -> [(TextWord, WordMap)] -> (TextWord, WordMap)
kvpm k v = (Literal k, WordMap . M.fromList $ v)

kvpc :: String -> Word -> (TextWord, WordMap)
kvpc k v = (Literal k, Count v)

fullMap :: WordMap
fullMap =
    WordMap $ M.fromList
        [ kvpm "0"
            [ kvpm "00"
                [ kvpm "000"
                    [ kvpc "0000" 3
                    , kvpm "0001"
                        [ kvpc "00010" 7
                        ]
                    ]
                , kvpm "001"
                    [ kvpc "0010" 2
                    , kvpm "0011"
                        [ kvpc "00110" 6
                        , kvpc "00111" 9
                        ]
                    ]
                ]
            , kvpc "01" 1
            ]
        , kvpm "1"
            [ kvpm "10"
                [ kvpc "100" 4
                , kvpc "101" 1
                ]
            , kvpc "11" 8
            ]
        ]

testAddToMap :: Spec
testAddToMap = parallel $
    describe "addToMap" $ do
        context "when map is empty" $ do
            it "can add no words" $
                addToMap emptyMap [] `shouldBe` emptyMap
            it "can add one word" $
                addToMap emptyMap [TextStart] `shouldBe`
                    (WordMap $ M.singleton TextStart $ Count 1)
            it "can add a sequence of words" $
                addToMap emptyMap [TextStart, Literal "a", Literal "b", Literal "c"] `shouldBe`
                    (WordMap $ M.singleton TextStart $
                        WordMap $ M.singleton (Literal "a") $
                            WordMap $ M.singleton (Literal "b") $
                                WordMap $ M.singleton (Literal "c") (Count 1))
        context "when map has elements in it" $ do
            it "can add no words" $
                addToMap fullMap [] `shouldBe` fullMap
            it "can add one word" $ do
                addToMap fullMap [TextStart] `shouldBe`
                    (WordMap $ M.fromList
                        [ kvpm "0"
                            [ kvpm "00"
                                [ kvpm "000"
                                    [ kvpc "0000" 3
                                    , kvpm "0001"
                                        [ kvpc "00010" 7
                                        ]
                                    ]
                                , kvpm "001"
                                    [ kvpc "0010" 2
                                    , kvpm "0011"
                                        [ kvpc "00110" 6
                                        , kvpc "00111" 9
                                        ]
                                    ]
                                ]
                            , kvpc "01" 1
                            ]
                        , kvpm "1"
                            [ kvpm "10"
                                [ kvpc "100" 4
                                , kvpc "101" 1
                                ]
                            , kvpc "11" 8
                            ]
                        , (TextStart, Count 1)
                        ])
                addToMap fullMap [Literal "0"] `shouldBe`
                    (WordMap $ M.fromList
                        [ kvpm "0"
                            [ kvpm "00"
                                [ kvpm "000"
                                    [ kvpc "0000" 3
                                    , kvpm "0001"
                                        [ kvpc "00010" 7
                                        ]
                                    ]
                                , kvpm "001"
                                    [ kvpc "0010" 2
                                    , kvpm "0011"
                                        [ kvpc "00110" 6
                                        , kvpc "00111" 9
                                        ]
                                    ]
                                ]
                            , kvpc "01" 1
                            ]
                        , kvpm "1"
                            [ kvpm "10"
                                [ kvpc "100" 4
                                , kvpc "101" 1
                                ]
                            , kvpc "11" 8
                            ]
                        ])
            it "can add a sequence of words" $ do
                addToMap fullMap [Literal "0", Literal "00", Literal "000", Literal "0000"] `shouldBe`
                    (WordMap $ M.fromList
                        [ kvpm "0"
                            [ kvpm "00"
                                [ kvpm "000"
                                    [ kvpc "0000" 4
                                    , kvpm "0001"
                                        [ kvpc "00010" 7
                                        ]
                                    ]
                                , kvpm "001"
                                    [ kvpc "0010" 2
                                    , kvpm "0011"
                                        [ kvpc "00110" 6
                                        , kvpc "00111" 9
                                        ]
                                    ]
                                ]
                            , kvpc "01" 1
                            ]
                        , kvpm "1"
                            [ kvpm "10"
                                [ kvpc "100" 4
                                , kvpc "101" 1
                                ]
                            , kvpc "11" 8
                            ]
                        ])
                addToMap fullMap [Literal "0", Literal "00", Literal "000", Literal "0002"] `shouldBe`
                    (WordMap $ M.fromList
                        [ kvpm "0"
                            [ kvpm "00"
                                [ kvpm "000"
                                    [ kvpc "0000" 3
                                    , kvpm "0001"
                                        [ kvpc "00010" 7
                                        ]
                                    , kvpc "0002" 1
                                    ]
                                , kvpm "001"
                                    [ kvpc "0010" 2
                                    , kvpm "0011"
                                        [ kvpc "00110" 6
                                        , kvpc "00111" 9
                                        ]
                                    ]
                                ]
                            , kvpc "01" 1
                            ]
                        , kvpm "1"
                            [ kvpm "10"
                                [ kvpc "100" 4
                                , kvpc "101" 1
                                ]
                            , kvpc "11" 8
                            ]
                        ])
                addToMap fullMap [Literal "1", Literal "11"] `shouldBe`
                    (WordMap $ M.fromList
                        [ kvpm "0"
                            [ kvpm "00"
                                [ kvpm "000"
                                    [ kvpc "0000" 3
                                    , kvpm "0001"
                                        [ kvpc "00010" 7
                                        ]
                                    ]
                                , kvpm "001"
                                    [ kvpc "0010" 2
                                    , kvpm "0011"
                                        [ kvpc "00110" 6
                                        , kvpc "00111" 9
                                        ]
                                    ]
                                ]
                            , kvpc "01" 1
                            ]
                        , kvpm "1"
                            [ kvpm "10"
                                [ kvpc "100" 4
                                , kvpc "101" 1
                                ]
                            , kvpc "11" 9
                            ]
                        ])

lexicon :: [String]
lexicon = map (:[]) ['a'..'z']

genWord :: Gen String
genWord = do
    i <- choose (0, length lexicon - 1)
    return $ lexicon!!i

textGen :: Bool -> Gen [String]
textGen allowEmptyLists = do
    size <- getSize
    l <- choose (0, size)
    let l' = if allowEmptyLists then l else l + 1
    vectorOf l' genWord

forAllTexts :: ([String] -> Word -> Bool) -> Property
forAllTexts = forAll $ textGen True

forAllNonEmptyTexts :: ([String] -> Word -> Bool) -> Property
forAllNonEmptyTexts = forAll $ textGen False

testWordMap :: (Word -> [String] -> WordMap -> Bool) -> [String] -> Word -> Bool
testWordMap p text numPreceding =
    p numPreceding text $ constructMap numPreceding text

foldMapWordMap :: ([a] -> b) -> (TextWord -> WordMap -> a) -> Map TextWord WordMap -> b
foldMapWordMap f p m =
    f mappedList
    where
        asList = M.toList m
        mappedList = map (uncurry p) asList

noEmptyMaps :: Word -> [String] -> WordMap -> Bool
noEmptyMaps numPreceding text (WordMap m) =
    M.null m || -- the root being null is OK
    foldMapWordMap and mapChildren m
    where
        mapChildren _ = noEmptyMapsRecurse numPreceding text
noEmptyMaps _ _ (Count _) = True

noEmptyMapsRecurse :: Word -> [String] -> WordMap -> Bool
noEmptyMapsRecurse numPreceding text (WordMap m) =
    not (M.null m) &&
    foldMapWordMap and mapChildren m
    where
        mapChildren _ = noEmptyMapsRecurse numPreceding text
noEmptyMapsRecurse _ _ (Count _) = True

depthValid :: Word -> [String] -> WordMap -> Bool
depthValid numPreceding = depthValidRecurse (fromIntegral numPreceding)

depthValidRecurse :: Int -> [String] -> WordMap -> Bool
depthValidRecurse numPreceding text (WordMap m) =
    foldMapWordMap and mapChildren m
    where
        mapChildren _ = depthValidRecurse (numPreceding - 1) text
depthValidRecurse numPreceding _ (Count _) = numPreceding == -1

noCount0 :: Word -> [String] -> WordMap -> Bool
noCount0 numPreceding text (WordMap m) =
    foldMapWordMap and mapChildren m
    where
        mapChildren _ = noCount0 numPreceding text
noCount0 _ _ (Count c) = c /= 0

allWordsInSourceText :: Word -> [String] -> WordMap -> Bool
allWordsInSourceText numPreceding text (WordMap m) =
    foldMapWordMap and mapChildren m
    where
        mapChildren key value =
            (show key `elem` text || show key == "<TextStart>") &&
            allWordsInSourceText numPreceding text value
allWordsInSourceText _ _ (Count _) = True

totalCountEqualsTextLength :: Word -> [String] -> WordMap -> Bool
totalCountEqualsTextLength _ text wm =
    getTotalCount wm == fromIntegral (length text)

getTotalCount :: WordMap -> Word
getTotalCount (WordMap m) =
    foldMapWordMap sum mapChildren m
    where
        mapChildren _ = getTotalCount
getTotalCount (Count c) = c

totalTextStartNodesEqualsNumPreceding :: Word -> [String] -> WordMap -> Bool
totalTextStartNodesEqualsNumPreceding numPreceding _ wm =
    getTotalTextStartNodes wm == fromIntegral numPreceding

getTotalTextStartNodes :: WordMap -> Word
getTotalTextStartNodes (WordMap m) =
    foldMapWordMap sum mapChildren m
    where
        mapChildren key value =
            getTotalTextStartNodes value +
            case key of
                TextStart -> 1
                Literal _ -> 0
getTotalTextStartNodes (Count _) = 0

testConstructMap :: Spec
testConstructMap = parallel $
    describe "constructMap" $ do
        it "doesn't contain empty maps" $ property $ forAllTexts $
            testWordMap noEmptyMaps
        it "contains all branches with depth == numPreceding + 1" $ property $ forAllTexts $
            testWordMap depthValid
        it "doesn't contain any (Count 0) nodes" $ property $ forAllTexts $
            testWordMap noCount0
        it "only contains words from the source text" $ property $ forAllTexts $
            testWordMap allWordsInSourceText
        it "has total count equal to number of words in text" $ property $ forAllTexts $
            testWordMap totalCountEqualsTextLength
        it "has numPreceding TextStart nodes" $ property $ forAllNonEmptyTexts $ -- only works with non-empty texts
            testWordMap totalTextStartNodesEqualsNumPreceding
