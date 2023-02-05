module Chap01Spec (spec) where

import Test.Hspec
import Chap01 (
        intMax, intMin,
        lengthReallyBigInteger,
        sampleDouble,
        valueTrue, valueFalse,
        valueChar, valueString,
        add
    )

spec :: Spec
spec = do
    describe "Type" $ do
        describe "Int" $ do
            it "intMax is max value of int" $
                intMax `shouldBe` 9223372036854775807
            it "intMin is min value of int" $
                intMin `shouldBe` -9223372036854775808
        describe "Integer" $ do
            it "Integer is limited only by the amount of memory on your machine." $
                lengthReallyBigInteger 2 `shouldBe` 19729
        describe "Double" $ do
            it "decimal notation" $
                sampleDouble `shouldBe` 0.00045387
            it "exponential notation" $
                sampleDouble `shouldBe` 4.5387e-4
        describe "Booleans" $ do
            it "True" $
                valueTrue `shouldBe` True
            it "False" $
                valueFalse `shouldBe` False
        describe "Char" $ do
            it "unicode character x" $
                valueChar 'x' `shouldBe` 'x'
            it "unicode character Ａ" $
                valueChar 'Ａ' `shouldBe` 'Ａ'
            it "unicode character あ" $
                valueChar 'あ' `shouldBe` 'あ'
        describe "String" $ do
            it "string sample" $
                valueString "Hello world ！！" `shouldBe` "Hello world ！！"
    describe "Arithmetic" $ do
        describe "Add" $ do
            it "3 + 2 = 5" $
                add 3 2 `shouldBe` 5
            it "(3 + 2) + 2 = 7" $
                add (add 3 2) 2 `shouldBe` 7
