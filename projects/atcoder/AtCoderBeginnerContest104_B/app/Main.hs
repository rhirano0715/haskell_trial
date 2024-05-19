-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc104/tasks/abc104_b
module Main where

import Data.Char (isLower , isUpper)

main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

-- | Solve the problem.
--
-- >>> solve "AtCoder"
-- "AC"
--
-- >>> solve "ACoder"
-- "WA"
--
-- >>> solve "AcycliC"
-- "WA"
--
-- >>> solve "AtCoCo"
-- "WA"
--
-- >>> solve "Atcoder"
-- "WA"
--
-- >>> solve "atCoder"
-- "WA"
--
-- >>> solve "AtCodeR"
-- "WA"
--
-- >>> solve "ATCoder"
-- "WA"
solve :: String -> String
solve s = if isAccepted s then "AC" else "WA"

-- | Check if the given string is accepted.
isAccepted :: String -> Bool
isAccepted s
    | length s < 3 = False
    | head s /= 'A' = False
    | not (hasSingleC middlePart) = False
    | not (all isLower remainingPart) = False
    | otherwise = True
  where
    middlePart = tail (init s)
    remainingPart = [head (tail s)] ++ [last s] ++ filter (/= 'C') middlePart

-- | Check if the given string has a single 'C'.
hasSingleC :: String -> Bool
hasSingleC s = length (filter (== 'C') s) == 1
