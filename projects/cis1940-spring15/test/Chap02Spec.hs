module Chap02Spec (spec) where

import Test.Hspec
import Chap02 (
        strLength
    )

spec :: Spec
spec = do
    describe "Polymorphism and Functional Programming Paradigms" $ do
        describe "Additional Syntax" $ do
            it "let expressions" $
                strLength "abc" `shouldBe` 3
