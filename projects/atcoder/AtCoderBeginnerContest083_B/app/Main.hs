-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc083/tasks/abc083_b
module Main where

main :: IO ()
main = do
    [n,a,b] <- map read . words <$> getLine
    print $ solve n a b

-- |
--
-- >>> solve 20 2 5
-- 84
--
-- >>> solve 10 1 2
-- 13
--
-- >>> solve 100 4 16
-- 4554
solve :: Int -> Int -> Int -> Int
solve n a b = sum $ genelateNumbers n a b

-- |
--
-- >>> genelateNumbers 20 2 5
-- [2,3,4,5,11,12,13,14,20]
--
-- >>> genelateNumbers 10 1 2
-- [1,2,10]
--
-- >>> genelateNumbers 100 4 16
-- [4,5,6,7,8,9,13,14,15,16,17,18,19,22,23,24,25,26,27,28,29,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,90,91,92,93,94,95,96,97]
--
genelateNumbers :: Int -> Int -> Int -> [Int]
genelateNumbers n a b = [x | x <- [1..n], let s = sumDigits x, a <= s && s <= b]

-- |
--
-- > sumDigits 123
-- 6
--
-- > sumDigits 100
-- 1
--
-- > sumDigits 2
-- 2
--
sumDigits :: Int -> Int
sumDigits = sum . map (read . return) . show
