module Chap02Spec (spec) where

import Test.Hspec
import Chap02 
-- (
--         strLength,
--         colors, Peg,
--         exactMatches,
--     )

spec :: Spec
spec = do
    describe "Polymorphism and Functional Programming Paradigms" $ do
        describe "Additional Syntax" $ do
            it "let expressions" $
                strLength "abc" `shouldBe` 3
    describe "Exercise" $ do
        describe "Exercise 1" $ do
            describe "exactMatches returns the number of exact matches between the secret code and the codebreaker’s guess." $ do
                it "No match at all." $
                    exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red] `shouldBe` 0
                it "partial match." $
                    exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] `shouldBe` 2
                it "exact match." $
                    exactMatches [Red, Blue, Green, Yellow] [Red, Blue, Green, Yellow] `shouldBe` 4

