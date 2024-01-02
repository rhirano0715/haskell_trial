-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc123/tasks/abc123_b
module Main where

import Control.Monad (replicateM)
import Data.List (permutations)

-- | Function to round up to the nearest 10
-- >>> roundUp10 120
-- 120
-- >>> roundUp10 121
-- 130
-- >>> roundUp10 129
-- 130
roundUp10 :: Int -> Int
roundUp10 x = (x + 9) `div` 10 * 10

-- | Function to round up to the nearest 10 for all elements except the last
-- >>> roundUp10s [29, 20, 7, 35, 120]
-- [30,20,10,40,120]
-- >>> roundUp10s [29, 20, 7, 120, 35]
-- [30,20,10,120,35]
roundUp10s :: [Int] -> [Int]
roundUp10s xs = init roundUp10all ++ [last xs]
  where
    roundUp10all = map roundUp10 xs

-- | Function to calculate total time for a given order of dishes
-- >>> totalTime [29, 20, 7, 35, 120]
-- 220
-- >>> totalTime [29, 20, 7, 120, 35]
-- 215
totalTime :: [Int] -> Int
totalTime = sum . roundUp10s

-- | Finding the minimum time from all permutations of dishes
-- >>> solve [29, 20, 7, 35, 120]
-- 215
-- >>> solve [101, 86, 119, 108, 57]
-- 481
-- >>> solve [123, 123, 123, 123, 123]
-- 643
solve :: [Int] -> Int
solve dishes = minimum $ map totalTime $ permutations dishes

-- Main function to find the minimum time
main :: IO ()
main = do
    -- Reading the cooking times for each dish
    dishes <- replicateM 5 readLn
    print $ solve dishes
