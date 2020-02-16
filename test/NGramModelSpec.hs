module NGramModelSpec (spec) where

import NGramModel

import Test.Hspec

spec :: Spec
spec = testTokenize

testTokenize :: Spec
testTokenize = parallel $
    describe "tokenize" $ do
        it "can tokenize empty strings" $
            tokenize "" `shouldBe` []
        it "can tokenize single words" $ do
            tokenize "a" `shouldBe` ["a"]
            tokenize "abcdef" `shouldBe` ["abcdef"]
        it "can tokenize with surrounding whitespace" $ do
            tokenize "  a" `shouldBe` ["a"]
            tokenize "a  " `shouldBe` ["a"]
            tokenize "  a  " `shouldBe` ["a"]
            tokenize "  abcdef" `shouldBe` ["abcdef"]
            tokenize "abcdef  " `shouldBe` ["abcdef"]
            tokenize "  abcdef  " `shouldBe` ["abcdef"]
        it "can tokenize long texts" $
            tokenize " a b c d e f g   h i  " `shouldBe` ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
