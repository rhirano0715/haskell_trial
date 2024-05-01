-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc093/tasks/abc093_b
module Main where

import Data.List (nub, sort)

-- | solve
--
-- >>> solve 3 8 2
-- [3,4,7,8]
--
-- >>> solve 4 8 3
-- [4,5,6,7,8]
--
-- >>> solve 2 9 100
-- [2,3,4,5,6,7,8,9]
solve :: Int -> Int -> Int -> [Int]
solve a b k = sort . nub $ take k [a..b] ++ take k [b,b-1..a]

main :: IO ()
main = do
    [a, b, k] <- map read . words <$> getLine
    mapM_ print $ solve a b k
