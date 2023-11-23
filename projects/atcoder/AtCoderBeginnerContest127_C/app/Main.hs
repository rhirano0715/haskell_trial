-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc127/tasks/abc127_c
module Main where

import Control.Monad (replicateM)

-- |
-- >>> solve 4 2 [(1,3),(2,4)]
-- 2
--
-- >>> solve 10 3 [(3,6),(5,7),(6,9)]
-- 1
--
-- >>> solve 100000 1 [(1, 100000)]
-- 100000
solve :: Int -> Int -> [(Int,Int)] -> Int
solve n m lrPairs = max 0 (minR - maxL + 1)
  where
    (maxL, minR) = foldr (\(l, r) (ml, mr) -> (max ml l, min mr r)) (1, n) lrPairs

main :: IO ()
main = do
    [n, m] <- map read . words <$> getLine
    lrPairs <- replicateM m $ do
        [l, r] <- map read . words <$> getLine
        return (l, r)

    print $ solve n m lrPairs
