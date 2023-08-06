-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/hitachi2020/tasks/hitachi2020_b
module Main where

import Control.Monad
import Data.List

-- |
-- >>> minimumCost 2 3 1 [3, 3] [3, 3, 3] [(1,2,1)]
-- 5
-- >>> minimumCost 1 1 2 [10] [10] [(1,1,5),(1,1,10)]
-- 10
-- >>> minimumCost 2 2 1 [3,5] [3,5] [(2,2,2)]
-- 6
minimumCost :: Int -> Int -> Int -> [Int] -> [Int] -> [(Int, Int, Int)] -> Int
minimumCost a b m fridge micro tickets = minimum $ (minimum fridge + minimum micro) : [fridge !! (x - 1) + micro !! (y - 1) - c | (x, y, c) <- tickets]

main :: IO ()
main = do
  [a, b, m] <- map read . words <$> getLine
  fridge <- map read . words <$> getLine
  micro <- map read . words <$> getLine
  tickets <- replicateM m $ do
    [x, y, c] <- map read . words <$> getLine
    return (x, y, c)
  print $ minimumCost a b m fridge micro tickets
