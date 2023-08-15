-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc081/tasks/abc081_b
module Main where

import Data.Bits

-- |
-- >>> solve [8,12,40]
-- 2
--
-- >>> solve [5,6,8,10]
-- 0
--
-- >>> solve [382253568, 723152896, 37802240, 379425024, 404894720, 471526144]
-- 8
solve :: [Int] -> Int
solve as = minimum $ map solveOne as

solveOne :: Int -> Int
solveOne a
  | even a = countTrailingZeros a
  | otherwise = 0

main :: IO ()
main = do
  _ <- getLine
  a <- fmap (map read . words) getLine
  print $ solve a
