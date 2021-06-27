module BacktrackingSpec (spec) where

import Backtracking

import Test.Hspec

spec :: Spec
spec = do
    testBacktracking

testBacktracking :: Spec
testBacktracking = parallel $ do
    describe "findLastCommonAncestor" $ do
        it "works with empty lists" $
            findLastCommonAncestor [] [] `shouldBe` (Nothing :: Maybe String)
        it "works with first list empty" $
            findLastCommonAncestor [] ["a"] `shouldBe` Nothing
        it "works with second list empty" $
            findLastCommonAncestor ["a"] [] `shouldBe` Nothing
        it "works with no common ancestors" $
            findLastCommonAncestor ["a", "b", "c", "d"] ["e", "f", "g", "h"] `shouldBe` Nothing
        it "works with common ancestor at the beginning" $
            findLastCommonAncestor ["a", "b", "c", "d"] ["a", "f", "g", "h"] `shouldBe` Just "a"
        it "works with all common ancestors" $
            findLastCommonAncestor ["a", "b", "c", "d"] ["a", "b", "c", "d"] `shouldBe` Just "d"
        it "works with all common ancestors first longer" $
            findLastCommonAncestor ["a", "b", "c", "d", "e"] ["a", "b", "c", "d"] `shouldBe` Just "d"
        it "works with all common ancestors second longer" $
            findLastCommonAncestor ["a", "b", "c", "d"] ["a", "b", "c", "d", "e"] `shouldBe` Just "d"
        it "works with later ancestors in first list" $
            findLastCommonAncestor ["a", "b", "c", "d", "z"] ["a", "b", "c", "d", "e", "f", "z"] `shouldBe` Just "d"
        it "works with later ancestors in second list" $
            findLastCommonAncestor ["a", "b", "c", "d", "e", "f", "z"] ["a", "b", "c", "d", "z"] `shouldBe` Just "d"
    describe "backtrackTo" $ do
        it "works with an empty list" $
            backtrackTo "a" [] `shouldBe` []
        it "works with a single-item list without the destination" $
            backtrackTo "a" ["b"] `shouldBe` []
        it "works with a list without the destination" $
            backtrackTo "a" ["b", "c", "d"] `shouldBe` []
        it "works with only the destination" $
            backtrackTo "a" ["a"] `shouldBe` ["a"]
        it "works with two items" $
            backtrackTo "a" ["a", "b"] `shouldBe` ["a"]
        it "works with the destination in the middle" $
            backtrackTo "a" ["z", "a", "b"] `shouldBe` ["a"]
        it "works with a long path" $
            backtrackTo "a" ["z", "a", "b", "c", "d", "e", "f", "g"] `shouldBe` ["f", "e", "d", "c", "b", "a"]
