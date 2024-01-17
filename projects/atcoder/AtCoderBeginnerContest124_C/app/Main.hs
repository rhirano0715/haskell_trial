-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc124/tasks/abc124_c
module Main where

main :: IO ()
main = do
  s <- getLine
  print $ minChanges s

-- | The 'minChanges' function calculates the minimum number of changes needed
-- to make the input string alternate between '0' and '1'.
--
-- >>> minChanges "000"
-- 1
--
-- >>> minChanges "10010010"
-- 3
--
-- >>> minChanges "0"
-- 0
minChanges :: String -> Int
minChanges s = minimum $ diffs s

-- | The 'diffs' function calculates the number of differences between the input string
-- and the generated patterns.
--
-- >>> diffs "000"
-- [1,2]
--
-- >>> diffs "10010010"
-- [5,3]
--
-- >>> diffs "0"
-- [0,1]
diffs :: String -> [Int]
diffs s = map (countDifferences s) generatePatterns

-- | The 'generatePatterns' function generates the patterns '01' and '10' in a cycle.
generatePatterns :: [String]
generatePatterns = cycle <$> ["01", "10"]

-- | The 'compareWithPattern' function compares each character of the input string
-- with the corresponding character of the pattern, and returns a list of Bool values
-- indicating whether the characters are different.
--
-- >>> compareWithPattern "000" "010"
-- [False,True,False]
--
-- >>> compareWithPattern "000" "101"
-- [True,False,True]
--
-- >>> compareWithPattern "000" "000"
-- [False,False,False]
compareWithPattern :: String -> String -> [Bool]
compareWithPattern = zipWith (/=)

-- | The 'filterDifferences' function filters out the 'False' values from the list
-- returned by the 'compareWithPattern' function, leaving only the 'True' values.
--
-- >>> filterDifferences "000" "010"
-- [True]
--
-- >>> filterDifferences "000" "101"
-- [True,True]
filterDifferences :: String -> String -> [Bool]
filterDifferences s pattern = filter id $ compareWithPattern s pattern

-- | The 'countDifferences' function counts the number of 'True' values in the list
-- returned by the 'filterDifferences' function, which represents the number of differences
-- between the input string and the pattern.
--
-- >>> countDifferences "000" "010"
-- 1
--
-- >>> countDifferences "000" "101"
-- 2
countDifferences :: String -> String -> Int
countDifferences s pattern = length $ filterDifferences s pattern
