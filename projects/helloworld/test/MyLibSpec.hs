module MyLibSpec (spec) where

import Test.Hspec
import MyLib (factorial)

spec :: Spec
spec = do
    describe "" $ do
        it "factorial 0" $
            factorial 1 `shouldBe` 1
        it "factorial 3" $
            factorial 3 `shouldBe` 6
