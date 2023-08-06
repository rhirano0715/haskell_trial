{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/hitachi2020/tasks/hitachi2020_b
module Main where

import Control.Monad
import Data.Array
import Data.List

-- | Calculate the minimum cost to buy one refrigerator and one microwave.
--   The function takes an array of refrigerator prices, an array of microwave prices, and a list of discount tickets.
--   Each ticket is a tuple that contains the indices of the refrigerator and microwave and the discount value.
--
--   Here are some examples:
--
--   >>> minimumCost (listArray (1, 2) [3, 3]) (listArray (1, 3) [3, 3, 3]) [(1, 2, 1)]
--   5
--
--   >>> minimumCost (listArray (1, 1) [10]) (listArray (1, 1) [10]) [(1, 1, 5), (1, 1, 10)]
--   10
--
--   >>> minimumCost (listArray (1, 2) [3, 5]) (listArray (1, 2) [3, 5]) [(2, 2, 2)]
--   6
minimumCost :: Array Int Int -> Array Int Int -> [(Int, Int, Int)] -> Int
minimumCost fridge micro tickets = minimum $ (minFridge + minMicro) : [fridge ! x + micro ! y - c | (x, y, c) <- tickets]
  where
    minFridge = minimum $ elems fridge
    minMicro = minimum $ elems micro

main :: IO ()
main = do
  [a, b, m] <- map read . words <$> getLine
  fridgeList <- map read . words <$> getLine
  microList <- map read . words <$> getLine
  let fridge = listArray (1, a) fridgeList
  let micro = listArray (1, b) microList
  tickets <- replicateM m $ do
    [x, y, c] <- map read . words <$> getLine
    return (x, y, c)
  print $ minimumCost fridge micro tickets
