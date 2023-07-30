-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc092/tasks/abc092_b
module Main where

import Control.Monad

-- | Calculate the number of chocolate pieces prepared at the beginning of the camp.
--
-- This function takes the number of days of the camp, a list representing the interval of each participant eating chocolates,
-- and the number of remaining chocolates on the last day, then calculates the total number of chocolates prepared at the beginning of the camp.
--
-- Example:
--
-- >>> calculateChocolates 7 1 [2, 5, 10]
-- 8
-- >>> calculateChocolates 8 20 [1, 10]
-- 29
-- >>> calculateChocolates 30 44 [26, 18, 81, 18, 6]
-- 56
calculateChocolates :: Int -> Int -> [Int] -> Int
calculateChocolates d x a = sum (map (\a -> (d - 1) `div` a + 1) a) + x

main :: IO ()
main = do
  -- Reads the number of participants.
  n <- readLn :: IO Int
  -- Reads the number of camp days and the number of remaining chocolates on the last day.
  [d, x] <- map read . words <$> getLine :: IO [Int]
  -- Reads the intervals at which each participant eats chocolates.
  a <- replicateM n readLn :: IO [Int]
  -- Prints the total number of chocolates prepared at the beginning of the camp.
  print $ calculateChocolates d x a
