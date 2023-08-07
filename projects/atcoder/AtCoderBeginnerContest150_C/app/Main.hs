-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc150/tasks/abc150_c
module Main where

import Data.List (delete, elemIndex, permutations)
import Data.Maybe (fromJust)

-- function to generate permutations
generatePerms :: (Eq a) => [a] -> [[a]]
generatePerms [] = [[]]
generatePerms xs = [x : ys | x <- xs, ys <- generatePerms (delete x xs)]

-- permutation position function
permPos :: (Eq a) => [a] -> [[a]] -> Int
permPos xs xss = fromJust (elemIndex xs xss) + 1

-- |
-- calculate difference function
--
-- >>> calcDifference 3 [1,3,2] [3,1,2]
-- 3
--
-- >>> calcDifference 8 [7,3,5,4,2,1,6,8] [3,8,2,5,4,6,7,1]
-- 17517
--
-- >>> calcDifference 3 [1,2,3] [1,2,3]
-- 0
calcDifference :: Int -> [Int] -> [Int] -> Int
calcDifference n p q = abs $ permPos p perms - permPos q perms
  where
    perms = generatePerms [1 .. n]

-- main function
main :: IO ()
main = do
  n <- readLn
  p <- fmap (map read . words) getLine
  q <- fmap (map read . words) getLine
  print $ calcDifference n p q
