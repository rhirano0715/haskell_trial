-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc153/tasks/abc153_d
module Main where

-- |
-- >>> attackCount 2
-- 3
--
-- >>> attackCount 4
-- 7
--
-- >>> attackCount 1000000000000
-- 1099511627775
attackCount :: Integer -> Integer
attackCount 1 = 1
attackCount h = 2 * attackCount (h `div` 2) + 1

-- メイン関数で入力を受け取り、結果を出力
main :: IO ()
main = do
  h <- readLn :: IO Integer
  print $ attackCount h
