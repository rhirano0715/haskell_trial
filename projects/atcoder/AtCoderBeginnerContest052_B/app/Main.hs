-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc052/tasks/abc052_b
module Main where

import Data.List (scanl')

-- |
-- >>> solve "IIDID"
-- 2
-- >>> solve "DDIDDII"
-- 0
solve :: String -> Int
solve s = maximum $ listing s

-- |
-- >>> listing "IIDID"
-- [0,1,2,1,2,1]
-- >>> listing "DDIDDII"
-- [0,-1,-2,-1,-2,-3,-2,-1]
listing :: String -> [Int]
listing s = scanl' (\x c -> if c == 'I' then x + 1 else x - 1) 0 s

main :: IO ()
main = do
  _ <- getLine
  s <- getLine
  print $ solve s
