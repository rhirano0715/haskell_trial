-- |
-- Module      : Main
-- Description : [D - Brick Break](https://atcoder.jp/contests/abc148/tasks/abc148_d)
module Main where

-- countBricks next as
--   next: the brick number we currently want to break
--   as  : a list of brick numbers
-- returns: the "next brick number we want to break" after traversing all bricks
countBricks :: Int -> [Int] -> Int
countBricks next [] = next
countBricks next (x:xs)
  | x == next = countBricks (next + 1) xs
  | otherwise = countBricks next xs

-- | solve is the function that solves the problem
-- >>> solve 3 [2, 1, 2]
-- 1
--
-- >>> solve 3 [2, 2, 2]
-- -1
--
-- >>> solve 10 [3, 1, 4, 1, 5, 9, 2, 6, 5, 3]
-- 7
--
-- >>> solve 1 [1]
-- 0
solve :: Int -> [Int] -> Int
solve n as = result
  where
    finalNext = countBricks 1 as
    result
      | finalNext == 1 = -1  -- could not even break brick #1
      | otherwise      = n - (finalNext - 1)  -- the number of bricks left

main :: IO ()
main = do
    n  <- readLn
    as <- map read . words <$> getLine
    print (solve n as)
