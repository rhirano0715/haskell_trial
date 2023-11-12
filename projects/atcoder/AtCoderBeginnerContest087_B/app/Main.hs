-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc087/tasks/abc087_b
module Main where

-- |
-- >>> countWays 2 2 2 100
-- 2
-- >>> countWays 5 1 0 150
-- 0
-- >>> countWays 30 40 50 6000
-- 213
countWays :: Int -> Int -> Int -> Int -> Int
countWays a b c x = length [(i,j,k) | i <- [0..a], j <- [0..b], k <- [0..c], 500*i + 100*j + 50*k == x]

main :: IO ()
main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  c <- readLn :: IO Int
  x <- readLn :: IO Int

  print $ countWays a b c x
