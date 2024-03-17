-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc140/tasks/abc140_c
module Main where

main :: IO ()
main = do
  getLine
  bs <- map read . words <$> getLine
  print $ solve bs

-- |
-- >>> solve [2, 5]
-- 9
--
-- >>> solve [3]
-- 6
--
-- >>> solve [0, 153, 10, 10, 23]
-- 53
solve :: [Int] -> Int
solve bs = sum $ zipWith min bs (tail bs) ++ [head bs, last bs]

