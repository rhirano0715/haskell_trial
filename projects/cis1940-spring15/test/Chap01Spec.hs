module Chap01Spec (spec) where

import Test.Hspec
import Chap01 (intMax, intMin)

spec :: Spec
spec = do
    describe "Chap01 Int" $ do
        it "intMax is max value of int" $
            intMax `shouldBe` 9223372036854775807
        it "intMin is min value of int" $
            intMin `shouldBe` -9223372036854775808
