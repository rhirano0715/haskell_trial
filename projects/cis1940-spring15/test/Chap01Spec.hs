module Chap01Spec (spec) where

import Test.Hspec
import Chap01 (
        intMax, intMin,
        lengthReallyBigInteger,
        sampleDouble,
        valueTrue, valueFalse,
        valueChar, valueString,
        add, subtraction,multiplication, division, intDivision,
        modulus, powerMultiplier,
        notEqual, lessThan, lessThanOrEqual, greaterThan, greaterThanOrEqual,
        sumtorial, hailstone, foo,
        sumPair,
        functionMultipleArguments,
        nums, range, range2,
        emptyList ,integerListHasSingleElement,
        integerListHasThreeElement, integerListHasTwoElement,
        hailstoneSeq, lengthIntList,
        sumEveryTwo, hailstoneLen,
        lastDigit, dropLastDigit,
        toRevDigits, doubleEveryOther
    )

spec :: Spec
spec = do
    describe "Introduction to Haskell" $ do
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
                it "String is Char list" $
                    "Hello" `shouldBe` ['H', 'e', 'l', 'l', 'o']
        describe "Arithmetic" $ do
            describe "Add" $ do
                it "3 + 2 = 5" $
                    add 3 2 `shouldBe` 5
                it "(3 + 2) + 2 = 7" $
                    add (add 3 2) 2 `shouldBe` 7
            describe "Subtraction" $ do
                it "3 - 2 = 1" $
                    subtraction 3 2 `shouldBe` 1
            describe "Multiplication" $ do
                it "3 x 2 = 6" $
                    multiplication 3 2 `shouldBe` 6
            describe "Division" $ do
                it "3 / 2 = 1.5. '/' is performs floating-point division only. " $
                    division 3 2 `shouldBe` 1.5
                it "3 `div` 2 = 1. `div` is integer division. " $
                    intDivision 3 2 `shouldBe` 1
            describe "Modulus" $ do
                it "mod 3 2 = 1" $
                    modulus 3 2 `shouldBe` 1
                it "3 mod 2 = 1" $
                    3 `modulus` 2 `shouldBe` 1
            describe "Power" $ do
                it "3 ^ 2 = 9" $
                    powerMultiplier 3 2 `shouldBe` 9
                it "3 ^ 3 = 27" $
                    powerMultiplier 3 3 `shouldBe` 27
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
                    notEqual 1 1 `shouldBe` False
                it "1 /= 2 is True" $ 
                    notEqual 1 2 `shouldBe` True
            describe "<" $ do
                it "1 < 0 is False" $ 
                    1 `lessThan` 0 `shouldBe` False
                it "1 < 1 is False" $ 
                    1 `lessThan` 1 `shouldBe` False
                it "1 < 2 is True" $ 
                    1 `lessThan` 2 `shouldBe` True
            describe "<=" $ do
                it "1 <= 0 is False" $ 
                    1 `lessThanOrEqual` 0 `shouldBe` False
                it "1 <= 1 is True" $ 
                    1 `lessThanOrEqual` 1 `shouldBe` True
                it "1 <= 2 is True" $ 
                    1 `lessThanOrEqual` 2 `shouldBe` True
            describe ">" $ do
                it "1 > 0 is True" $ 
                    1 `greaterThan` 0 `shouldBe` True
                it "1 > 1 is False" $ 
                    1 `greaterThan` 1 `shouldBe` False
                it "1 > 2 is False" $ 
                    1 `greaterThan` 2 `shouldBe` False
            describe ">=" $ do
                it "1 >= 0 is True" $ 
                    1 `greaterThanOrEqual` 0 `shouldBe` True
                it "1 >= 1 is True" $ 
                    1 `greaterThanOrEqual` 1 `shouldBe` True
                it "1 >= 2 is False" $ 
                    1 `greaterThanOrEqual` 2 `shouldBe` False
        describe "Defining basic functions" $ do
            describe "sumtorial  compute the sum of the integers from 1 to n. using matching" $ do
                it "sumtorial 0 = 0" $
                    sumtorial 0 `shouldBe` 0
                it "sumtorial 3 = 6" $
                    sumtorial 3 `shouldBe` 6
            describe "hailstone . using guards" $ do
                it "hailstone 0 = 0" $
                    hailstone 0 `shouldBe` 0
                it "hailstone 2 = 1" $
                    hailstone 2 `shouldBe` 1
                it "hailstone 1 = 4" $
                    hailstone 1 `shouldBe` 4
            describe "foo . using matching and guards" $ do
                it "foo 0" $
                    foo 0 `shouldBe` 16
                it "foo 1" $
                    foo 1 `shouldBe` 3
                it "foo -3" $
                    foo (-3) `shouldBe` 0
                it "foo 36" $
                    foo 36 `shouldBe` -43
                it "foo 38" $
                    foo 38 `shouldBe` 41
        describe "Pair" $ do
            it "wip" $
                sumPair(1, 2) `shouldBe` 3
        describe "Using functions, and multiple arguments" $ do
            it "functionMultipleArguments 1 2 3" $
                functionMultipleArguments 1 2 3 `shouldBe` 6
            it "functionMultipleArguments 1 (2 * 3) 3" $
                functionMultipleArguments 1 (2 * 3) 3 `shouldBe` 10
        describe "List" $ do
            it "nums" $
                nums `shouldBe` [1,2,3,19]
            it "range" $
                range `shouldBe` [1..100]
            it "Isometric sequence of 2, from 2 to 100" $
                range2 `shouldBe` [2,4..100]
        describe "List" $ do
            it "String are just list of Char's" $
                "hello" `shouldBe` ['h', 'e', 'l', 'l', 'o']
            it "Empty list" $ 
                emptyList `shouldBe` []
            it "Create a list has single element" $
                integerListHasSingleElement `shouldBe` [1]
            it "Create a list has two element" $
                integerListHasTwoElement `shouldBe` [3,1]
            it "Create a list has three element" $
                integerListHasThreeElement `shouldBe` [2,3,4]
            it "Function return list" $
                hailstoneSeq 17 `shouldBe` [17,52,26,13,40,20,10,5,16,8,4,2,1]
            it "Function argument is list" $
                lengthIntList [1,2,4] `shouldBe` 3
            it "Function argument is list, return list" $
                sumEveryTwo [5,4,3,2,1] `shouldBe` [9, 5, 1]
        describe "Combining functions" $ do
            it "hailstoneLen returns hailstoneSeq's Length" $
                hailstoneLen 9 `shouldBe` 19
    describe "Homework 1" $ do
        describe "Exercise 1" $ do
            describe "lastDigit extract the last digit" $ do
                it "123 -> 3" $
                    lastDigit 123 `shouldBe` 3
                it "1234 -> 4" $
                    lastDigit 1234 `shouldBe` 4
                it "5 -> 5" $
                    lastDigit 5 `shouldBe` 5
                it "10 -> 0" $
                    lastDigit 10 `shouldBe` 0
                it "0 -> 0" $
                    lastDigit 0 `shouldBe` 0
            describe "dropLastDigit" $ do
                it "123 -> 12" $
                    dropLastDigit 123 `shouldBe` 12
                it "1234 -> 123" $
                    dropLastDigit 123 `shouldBe` 12
                it "5 -> 0" $
                    dropLastDigit 5 `shouldBe` 0
                it "10 -> 1" $
                    dropLastDigit 10 `shouldBe` 1
                it "0 -> 0" $
                    dropLastDigit 0 `shouldBe` 0
        describe "Exercise 2" $ do
            describe "toRevDigits convert integer to a list of its digits in reverser order." $ do
                it "1234 -> [4, 3, 2, 1]" $
                    toRevDigits 1234 `shouldBe` [4, 3, 2, 1]
                it "53471 -> [1, 7, 4, 3, 5]" $
                    toRevDigits 53471 `shouldBe` [1, 7, 4, 3, 5]
                it "0 -> []" $
                    toRevDigits 0 `shouldBe` []
                it "-1 -> []" $
                    toRevDigits (-1) `shouldBe` []
        describe "Exercise 3" $ do
            describe "The function doubleEveryOther double every other number starting with the second one" $ do
                it " [4, 9, 5, 5] = [4, 18, 5, 10]" $
                    doubleEveryOther [4, 9, 5, 5] `shouldBe` [4, 18, 5, 10]
                it " [0, 0] = [0, 0]" $
                    doubleEveryOther [0, 0] `shouldBe` [0, 0]
                it " [3] = [3]" $
                    doubleEveryOther [3] `shouldBe` [3]
                it " [3, 4] = [3, 4]" $
                    doubleEveryOther [3, 4] `shouldBe` [3, 8]
