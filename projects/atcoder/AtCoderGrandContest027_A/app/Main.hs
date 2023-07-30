-- \|
-- Module      : Main
-- Description : https://atcoder.jp/contests/agc027/tasks/agc027_a
module Main where

import Control.Monad
import Data.List

-- |
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

main :: IO ()
main = do
  [n, x] <- map read . words <$> getLine
  a <- sort . map read . words <$> getLine
  -- print $ solve x a
  print $ distribute n x a
