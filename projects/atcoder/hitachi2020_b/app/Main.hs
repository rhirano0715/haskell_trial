{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/hitachi2020/tasks/hitachi2020_b
module Main where

import Control.Monad
import Data.Array
import Data.List

-- | 'minimumCost' calculates the minimum cost to buy a fridge and a microwave.
-- It takes an array of fridge prices, an array of microwave prices, and a list of discount tickets.
-- Each ticket is a tuple of fridge type, microwave type, and discount amount.
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
minimumCost fridgePrices microPrices tickets = minimum allPossibleCosts
  where
    minFridgePrice = minimum $ elems fridgePrices -- Minimum price among all fridges
    minMicroPrice = minimum $ elems microPrices -- Minimum price among all microwaves
    costWithoutTicket = minFridgePrice + minMicroPrice -- Cost without using any ticket
    costWithTicket = [fridgePrices ! fridgeType + microPrices ! microType - discount | (fridgeType, microType, discount) <- tickets] -- Costs using each ticket
    allPossibleCosts = costWithoutTicket : costWithTicket -- All possible costs, with or without tickets

-- | 'main' function reads the input, applies the 'minimumCost' function, and prints the result.
main :: IO ()
main = do
  -- Read the numbers of fridge types, microwave types, and tickets
  [numFridgeTypes, numMicroTypes, numTickets] <- map read . words <$> getLine
  -- Read the price of each fridge type
  fridgePriceList <- map read . words <$> getLine
  -- Read the price of each microwave type
  microPriceList <- map read . words <$> getLine
  -- Create arrays for the prices
  let fridgePrices = listArray (1, numFridgeTypes) fridgePriceList
  let microPrices = listArray (1, numMicroTypes) microPriceList
  -- Read each ticket
  tickets <- replicateM numTickets $ do
    [fridgeType, microType, discount] <- map read . words <$> getLine
    return (fridgeType, microType, discount)
  -- Calculate and print the minimum cost
  print $ minimumCost fridgePrices microPrices tickets
