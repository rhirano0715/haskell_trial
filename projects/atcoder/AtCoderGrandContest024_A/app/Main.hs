-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/agc024/tasks/agc024_a
module Main where

main :: IO ()
main = do
  [a, b, c, k] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ solve a b c k

-- | solve the problem
--
-- >>> solve 1 2 3 1
-- "1"
--
-- >>> solve 2 3 2 0
-- "-1"
--
-- >>> solve 1000000000 1000000000 1000000000 1000000000000000000
-- "0"
--
solve :: Int -> Int -> Int -> Int -> String
solve a b c k = if result > 10^18 then "Unfair" else show result
  where
    result = eval a b c k

eval :: Int -> Int -> Int -> Int -> Int
eval a b c k
  | odd k = b - a
  | otherwise = a - b

