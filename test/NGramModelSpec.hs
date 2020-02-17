module NGramModelSpec (spec) where

import NGramModel

import qualified Data.Map.Strict as M (empty, singleton, fromList)

import Test.Hspec

spec :: Spec
spec = testAddToMap

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
