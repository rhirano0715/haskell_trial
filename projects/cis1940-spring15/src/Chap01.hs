module Chap01 (
        intMax, intMin,
        lengthReallyBigInteger,
        sampleDouble,
        valueTrue, valueFalse,
        valueChar, valueString,
        add, subtraction, multiplication, division,
        sumtorial, hailstone, foo,
        sumPair,
        functionMultipleArguments,
        nums, range, range2,
        emptyList ,integerListHasSingleElement,
        integerListHasThreeElement, integerListHasTwoElement,
        hailstoneSeq, 
        sumEveryTwo, hailstoneLen,
        lastDigit, dropLastDigit,
        toRevDigits, doubleEveryOther
    ) where

-- |
-- intMax
-- >>> intMax
-- 9223372036854775807
intMax :: Int
intMax = maxBound

-- |
-- intMin
-- >>> intMin
-- -9223372036854775808
intMin :: Int
intMin = minBound

reallyBigInteger :: Integer -> Integer
reallyBigInteger n = n ^ ( n ^ ( n ^ ( n ^ n ) ) )


-- |
-- lengthReallyBigInteger
-- >>> lengthReallyBigInteger 2
-- 19729
lengthReallyBigInteger :: Integer -> Int
lengthReallyBigInteger n = length(show (reallyBigInteger n))

sampleDouble :: Double
sampleDouble = 4.5387e-4

valueTrue :: Bool
valueTrue = True

valueFalse :: Bool
valueFalse = False

valueChar :: Char -> Char
valueChar c = c

valueString :: String -> String
valueString s = s

-- |
-- add
-- >>> add 2 1
-- 3
-- >>> add 123 1
-- 124
add :: Int -> Int -> Int
add x y = x + y

subtraction :: Int -> Int -> Int
subtraction x y = x - y

multiplication :: Int -> Int -> Int
multiplication x y = x * y

division :: Double -> Double -> Double
division x y = x / y

-- |
-- sumtorial
-- >>> sumtorial 3
-- 6
-- >>> sumtorial 2
-- 3
-- >>> sumtorial 1
-- 1
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

-- |
-- hailstone
-- >>> hailstone 1
-- 4
-- >>> hailstone 2
-- 1
-- >>> hailstone 3
-- 10
-- >>> hailstone 4
-- 2
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

-- |
-- foo
-- >>> foo 0
-- 16
-- >>> foo 1
-- 3
-- >>> foo 2
-- -43
-- >>> foo 16
-- 19
-- >>> foo 17
-- 20
-- >>> foo 18
-- 21
-- >>> foo 19
-- -43
foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3

sumPair :: (Int,Int) -> Int
sumPair (x,y) = x + y

functionMultipleArguments :: Int -> Int -> Int -> Int
functionMultipleArguments x y z = x + y + z

nums, range, range2 :: [Int]
nums   = [1,2,3,19]
range  = [1..100]
range2 = [2,4..100]

emptyList :: [Int]
emptyList = []

integerListHasSingleElement :: [Int]
integerListHasSingleElement = 1 : []

integerListHasTwoElement :: [Int]
integerListHasTwoElement = 3 : (1 : [])

integerListHasThreeElement :: [Int]
integerListHasThreeElement = 2 : 3 : 4 : []

-- Generate the sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

hailstoneLen :: Integer -> Int
hailstoneLen n = length (hailstoneSeq n) - 1

-- 
-- The following code causes a compile error
-- 
--     functionsThatCauseCompilationErrors :: String
--     functionsThatCauseCompilationErrors = 'x' ++ "foo"
-- 
-- The error message is as follows:
-- 
--     src/Chap01.hs:128:39: error:
--         • Couldn't match expected type ‘[Char]’ with actual type ‘Char’
--         • In the first argument of ‘(++)’, namely ‘'x'’
--           In the expression: 'x' ++ "foo"
--           In an equation for ‘functionsThatCauseCompilationErrors’:
--               functionsThatCauseCompilationErrors = 'x' ++ "foo"
--         |
--     128 | functionsThatCauseCompilationErrors = 'x' ++ "foo"
--         |                                       ^^^
-- 
-- Cause of Error
-- 
--     `++` requires `String` to the left and right. But right is Char
--     


lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

toRevDigits :: Integer -> [Integer]
toRevDigits n 
  | n <= 0    = []
  | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []  -- Do nothing to the empty list
doubleEveryOther (x:[])     = [x] -- Do nothing to lists with a single element
doubleEveryOther (x:(y:zs)) = x : (y * 2) : doubleEveryOther zs
