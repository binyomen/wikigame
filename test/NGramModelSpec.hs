module NGramModelSpec (spec) where

import NGramModel

import qualified Data.Map.Strict as M (empty, singleton)

import Test.Hspec

spec :: Spec
spec = testTokenize

emptyMap :: WordMap
emptyMap = WordMap M.empty

testTokenize :: Spec
testTokenize = parallel $
    describe "addToMap" $
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
