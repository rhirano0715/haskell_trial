-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc081/tasks/abc081_b
module Main where

import Data.Char
import Numeric

-- |
-- >>> solve 3 [8,12,40]
-- 2
--
-- >>> solve 4 [5,6,8,10]
-- 0
--
-- >>> solve 6 [382253568, 723152896, 37802240, 379425024, 404894720, 471526144]
-- 8
solve :: Int -> [Int] -> Int
solve _ [] = 0
solve _ as = minimum $ map solveOne as

solveOne :: Int -> Int
solveOne a
  | even a = 1 + solveOne dividedA
  | otherwise = 0
  where
    dividedA = a `div` 2

main :: IO ()
main = do
  n <- readLn
  a <- fmap (map read . words) getLine
  print $ solve n a
