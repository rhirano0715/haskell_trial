-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc133/tasks/abc133_b
module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, d] <- map read . words <$> getLine :: IO [Int]
  points <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  print $ solve points

solve :: [[Int]] -> Int
solve points = length $ filter isIntegerDistance $ combinations 2 points

isIntegerDistance :: [[Int]] -> Bool
isIntegerDistance [p1, p2] = let dist = sqrt . fromIntegral . sum $ zipWith (\x y -> (x - y) ^ 2) p1 p2
                             in dist == fromIntegral (round dist)

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations k (x:xs) = (map (x:) (combinations (k-1) xs)) ++ (combinations k xs)
