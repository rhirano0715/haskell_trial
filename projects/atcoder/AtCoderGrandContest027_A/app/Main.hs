-- \|
-- Module      : Main
-- Description : This module provides a solution to a problem of candy distribution to maximize the number of happy children.
--               https://atcoder.jp/contests/agc027/tasks/agc027_a

module Main where

import Control.Monad
import Data.List

-- |
-- The 'distribute' function calculates the maximum number of children that can be made happy.
-- Each child wants a certain number of candies and is happy when they get exactly that amount.
-- The candies are distributed in an order that maximizes the total number of happy children.
--
-- Example usage:
-- >>> distribute 3 70 [10,20,30]
-- 2
-- >>> distribute 3 10 [10,20,30]
-- 1
-- >>> distribute 4 1111 [1, 10, 100, 1000]
-- 4
-- >>> distribute 2 10 [20,20]
-- 0
distribute :: Int -> Int -> [Int] -> Int
distribute _ _ [] = 0
distribute n x (a : as)
  | isLast && x == a = 1
  | not isLast && x >= a = 1 + distribute (n - 1) (x - a) as
  | otherwise = 0
  where
    isLast = n == 1

-- |
-- The 'main' function parses input, applies the 'distribute' function and prints the result.
main :: IO ()
main = do
  [n, x] <- map read . words <$> getLine
  a <- sort . map read . words <$> getLine
  print $ distribute n x a
