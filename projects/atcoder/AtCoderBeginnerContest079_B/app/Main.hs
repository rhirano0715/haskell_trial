-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc079/tasks/abc079_b
module Main where

main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ lucasNumber n

-- | N-th Lucas number
--
-- >>> lucasNumber 5
-- 11
--
-- >>> lucasNumber 86
-- 939587134549734843
lucasNumber :: Int -> Int
lucasNumber n = fst $ foldl (\(a, b) _ -> (b, a + b)) (2, 1) [1 .. n]
