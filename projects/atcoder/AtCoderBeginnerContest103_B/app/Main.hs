-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc103/tasks/abc103_b
module Main where

import Data.List (isInfixOf)

isRotatedVersion :: String -> String -> Bool
isRotatedVersion s t = t `isInfixOf` (s ++ s)

-- |
-- >>> solve "kyoto" "tokyo"
-- "Yes"
--
-- >>> solve "abc" "arc"
-- "No"
--
-- >>> solve "aaaaaaaaaaaaaaab" "aaaaaaaaaaaaaaab"
-- "Yes"
solve :: String -> String -> String
solve s t = if isRotatedVersion s t then "Yes" else "No"

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  putStrLn $ solve s t
