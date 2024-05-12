-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc158/tasks/abc158_c
module Main where

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ solve a b

-- | solve
--
-- >>> solve 2 2
-- 25
--
-- >>> solve 8 10
-- 100
--
-- >>> solve 19 99
-- -1
solve :: Int -> Int -> Int
solve a b = if null results then -1 else head results
  where
    results = [ x | x <- [1..1000], (x * 8) `div` 100 == a, (x * 10) `div` 100 == b ]
