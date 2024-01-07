-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc152/tasks/abc152_c
module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  ps <- map read . words <$> getLine :: IO [Int]

  print $ solve n ps

-- | Solve the problem
--
-- >>> solve 5 [4, 2, 5, 1, 3]
-- 3
--
-- >>> solve 4 [4, 3, 2, 1]
-- 4
--
-- >>> solve 6 [1, 2, 3, 4, 5, 6]
-- 1
--
-- >>> solve 8 [5, 7, 4, 2, 6, 8, 1, 3]
-- 4
--
-- >>> solve 1 [1]
-- 1
solve :: Int -> [Int] -> Int
solve n = countTrue

-- | Return the minimum value of the list
--
-- >>> minimums [4, 2, 5, 1, 3]
-- [4,2,2,1,1]
--
-- >>> minimums [4, 3, 2, 1]
-- [4,3,2,1]
--
-- >>> minimums [1, 2, 3, 4, 5, 6]
-- [1,1,1,1,1,1]
--
-- >>> minimums [5, 7, 4, 2, 6, 8, 1, 3]
-- [5,5,4,2,2,2,1,1]
--
-- >>> minimums [1]
-- [1]
minimums :: [Int] -> [Int]
minimums = scanl1 min

-- | Return the list of Bool
--
-- >>> conditions [4, 2, 5, 1, 3]
-- [True,True,False,True,False]
--
-- >>> conditions [4, 3, 2, 1]
-- [True,True,True,True]
--
-- >>> conditions [1, 2, 3, 4, 5, 6]
-- [True,False,False,False,False,False]
--
-- >>> conditions [5, 7, 4, 2, 6, 8, 1, 3]
-- [True,False,True,True,False,False,True,False]
--
-- >>> conditions [1]
-- [True]
conditions :: [Int] -> [Bool]
conditions ps = zipWith (>=) (minimums ps) ps

-- | Return the number of True
--
-- >>> countTrue [4, 2, 5, 1, 3]
-- 3
--
-- >>> countTrue [4, 3, 2, 1]
-- 4
--
-- >>> countTrue [1, 2, 3, 4, 5, 6]
-- 1
--
-- >>> countTrue [5, 7, 4, 2, 6, 8, 1, 3]
-- 4
--
-- >>> countTrue [1]
-- 1
countTrue :: [Int] -> Int
countTrue = length . filter id . conditions
