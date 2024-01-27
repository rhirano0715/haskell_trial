-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc090/tasks/abc090_b
module Main where

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  print $ solve a b

-- | solve the problem
--
-- >>> solve 11009 11332
-- 4
--
-- >>> solve 31415 92653
-- 612
solve :: Int -> Int -> Int
solve a b = length $ filter isPalindrome [a .. b]

-- | check if the given number is palindrome
--
-- >>> isPalindrome 12321
-- True
--
-- >>> isPalindrome 12345
-- False
isPalindrome :: Int -> Bool
isPalindrome n = show n == reverse (show n)
