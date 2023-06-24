module Main where

import Control.Monad

main :: IO ()
main = do
    [_, m, x] <- map read . words <$> getLine :: IO [Int]
    a <- map read . words <$> getLine :: IO [Int]
    print $ tollGates m x a

-- |
-- >>> tollGates 3 3 [1, 2, 4]
-- 1
-- >>> tollGates 3 2 [4, 5, 6]
-- 0
-- >>> tollGates 7 5 [1, 2, 3, 4, 6, 8, 9]
-- 3
tollGates :: Int -> Int -> [Int] -> Int
tollGates m x a = min left right
  where
    left = length $ takeWhile (< x) a
    right = m - left 
