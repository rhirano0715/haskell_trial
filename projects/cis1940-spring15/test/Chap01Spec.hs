module Chap01Spec (spec) where

import Test.Hspec
import Chap01 (intMax, intMin, lengthReallyBigInteger, sampleDouble, valueTrue, valueFalse)

spec :: Spec
spec = do
    describe "Chap01 Int" $ do
        it "intMax is max value of int" $
            intMax `shouldBe` 9223372036854775807
        it "intMin is min value of int" $
            intMin `shouldBe` -9223372036854775808
    describe "Chap01 Integer" $ do
        it "Integer is limited only by the amount of memory on your machine." $
            lengthReallyBigInteger 2 `shouldBe` 19729
    describe "Chap01 Double" $ do
        it "decimal notation" $
            sampleDouble `shouldBe` 0.00045387
        it "exponential notation" $
            sampleDouble `shouldBe` 4.5387e-4
    describe "Chap01 Booleans" $ do
        it "True" $
            valueTrue `shouldBe` True
        it "False" $
            valueFalse `shouldBe` False
