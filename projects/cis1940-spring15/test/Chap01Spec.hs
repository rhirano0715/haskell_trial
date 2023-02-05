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
        describe "Subtraction" $ do
            it "3 - 2 = 1" $
                3 - 2 `shouldBe` 1
        describe "Multiplication" $ do
            it "3 x 2 = 6" $
                3 * 2 `shouldBe` 6
        describe "Division" $ do
            it "3 / 2 = 1.5. '/' is performs floating-point division only. " $
                3 / 2 `shouldBe` 1.5
            it "3 `div` 2 = 1. `div` is integer division. " $
                3 `div` 2 `shouldBe` 1
        describe "Modulus" $ do
            it "mod 3 2 = 1" $
                mod 3 2 `shouldBe` 1
            it "3 `mod` 2 = 1" $
                3 `mod` 2 `shouldBe` 1
        describe "Power" $ do
            it "3 ^ 2 = 9" $
                3 ^ 2 `shouldBe` 9
            it "3 ^ 3 = 27" $
                3 ^ 3 `shouldBe` 27
    describe "Boolean logic" $ do
        describe "AND" $ do
            it "True && False is False" $
                (True && False) `shouldBe` False
            it "False && True is False" $
                (False && True) `shouldBe` False
        describe "OR" $ do
            it "True || False is True" $
                (True || False) `shouldBe` True
            it "False || True is True" $
                (False || True) `shouldBe` True
        describe "NOT" $ do
            it "not True is False" $
                not True `shouldBe` False
            it "not True is True" $
                not False `shouldBe` True
        describe "==" $ do
            it "'a' == 'b' is True" $ 
                'a' == 'b' `shouldBe` False
            it "'a' == 'b' is False" $ 
                'a' == 'b' `shouldBe` False
        describe "/=" $ do
            it "1 /= 1 is False" $ 
                1 /= 1 `shouldBe` False
            it "1 /= 2 is True" $ 
                1 /= 2 `shouldBe` True
        describe "<" $ do
            it "1 < 0 is False" $ 
                1 < 0 `shouldBe` False
            it "1 < 1 is False" $ 
                1 < 1 `shouldBe` False
            it "1 < 2 is True" $ 
                1 < 2 `shouldBe` True
        describe "<=" $ do
            it "1 <= 0 is False" $ 
                1 <= 0 `shouldBe` False
            it "1 <= 1 is True" $ 
                1 <= 1 `shouldBe` True
            it "1 <= 2 is True" $ 
                1 <= 2 `shouldBe` True
        describe ">" $ do
            it "1 > 0 is True" $ 
                1 > 0 `shouldBe` True
            it "1 > 1 is False" $ 
                1 > 1 `shouldBe` False
            it "1 > 2 is False" $ 
                1 > 2 `shouldBe` False
        describe ">=" $ do
            it "1 >= 0 is True" $ 
                1 >= 0 `shouldBe` True
            it "1 >= 1 is True" $ 
                1 >= 1 `shouldBe` True
            it "1 >= 2 is False" $ 
                1 >= 2 `shouldBe` False
