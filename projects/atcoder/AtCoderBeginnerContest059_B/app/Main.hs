-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc059/tasks/abc059_b
module Main where

main :: IO ()
main = do
  a <- readLn :: IO Integer
  b <- readLn :: IO Integer
  putStrLn $ solve a b

-- | solve the problem
--
-- >>> solve 2 1
-- "GREATER"
--
-- >>> solve 1 2
-- "LESS"
--
-- >>> solve 57 57
-- "EQUAL"
--
-- >>> solve 123456789012345678901234567890 234567890123456789012345678901
-- "LESS"
solve :: Integer -> Integer -> String
solve a b
  | a > b = "GREATER"
  | a < b = "LESS"
  | otherwise = "EQUAL"
