-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc118/tasks/abc118_b
module Main where

import Control.Monad (replicateM)
import Data.List (intersect)

-- Main function
main :: IO ()
main = do
  -- Read the number of people and the number of types of food
  [n, _] <- map read . words <$> getLine
  
  -- Read the list of each person's favorite foods
  favoriteFoodsLists <- replicateM n (map read . tail . words <$> getLine) :: IO [[Int]]

  -- Output the result
  print $ countFoodsLovedByEveryone favoriteFoodsLists

-- | Function that takes a list of each person's favorite foods and returns the number of foods common to everyone
--
-- >>> countFoodsLovedByEveryone [[1,3],[1,2,3],[3,2]]
-- 1
--
-- >>> countFoodsLovedByEveryone [[2,3,4,5],[1,3,4,5],[1,2,4,5],[1,2,3,5],[1,2,3,4]]
-- 0
--
-- >>> countFoodsLovedByEveryone [[5,10,30]]
-- 3
countFoodsLovedByEveryone :: [[Int]] -> Int
countFoodsLovedByEveryone favoriteFoodsLists = length $ foodsLovedByEveryone favoriteFoodsLists

-- | Function that takes a list of each person's favorite foods and returns the foods common to everyone
--
-- >>> foodsLovedByEveryone [[1,3],[1,2,3],[3,2]]
-- [3]
--
-- >>> foodsLovedByEveryone [[2,3,4,5],[1,3,4,5],[1,2,4,5],[1,2,3,5],[1,2,3,4]]
-- []
--
-- >>> foodsLovedByEveryone [[5,10,30]]
-- [5,10,30]
foodsLovedByEveryone :: [[Int]] -> [Int]
foodsLovedByEveryone [] = []
foodsLovedByEveryone (firstPersonFoods:rest) = foldl intersect firstPersonFoods rest

