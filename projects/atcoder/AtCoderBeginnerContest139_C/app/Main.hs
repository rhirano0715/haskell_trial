-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc139/tasks/abc139_c
module Main where

main :: IO ()
main = do
  _ <- readLn :: IO Int
  heights <- map read . words <$> getLine :: IO [Int]

  print $ solve heights

-- | solve
--   Problem Statement
--   There are N squares arranged in a row from left to right.
--   The height of the i-th square from the left is H i.
--   You will land on a square of your choice, then repeat moving to the adjacent square on the right as long as the height of the next square is not greater than that of the current square.
--   Find the maximum number of times you can move.
--
-- >>> solve [10, 4, 8, 7, 3]
-- 2
--
-- >>> solve [4, 4, 5, 6, 6, 5, 5]
-- 3
--
-- >>> solve [1, 2, 3, 4]
-- 0
solve :: [Int] -> Int
solve heights = maximum $ countDown 0 heights
  where
    countDown _ []       = []
    countDown cnt [x] = [cnt]
    countDown cnt (x:y:xs)
      | y <= x    = countDown (cnt + 1) (y:xs)
      | otherwise = cnt : countDown 0 (y:xs)
