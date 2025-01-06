-- |
-- Module      : Main
-- Description : [B - ∵∴∵](https://atcoder.jp/contests/abc058/tasks/abc058_b)
module Main where

main :: IO ()
main = do
  xs <- getLine
  ys <- getLine
  putStrLn $ solve xs ys

-- | solve is the function that solves the problem
-- >>> solve "xyz" "abc"
-- "xaybzc"
--
-- >>> solve "atcoderbeginnercontest" "atcoderregularcontest"
-- "aattccooddeerrbreeggiunlnaerrccoonntteesstt"
solve :: String -> String -> String
solve [] _ = []
solve (o:os) [] = o: os
solve (o:os) (e:es) = o : e : solve os es
