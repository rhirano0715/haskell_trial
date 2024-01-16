-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc044/tasks/abc044_b
module Main where

import qualified Data.Map as Map

main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

-- | Solve the problem
--
-- >>> solve "abaccaba"
-- "Yes"
--
-- >>> solve "hthth"
-- "No"
solve :: String -> String
solve s = if  doAllCharsAppearEvenNumberOfTimes s then "Yes" else "No"

-- | Check if all characters appear even number of times
--
-- >>> doAllCharsAppearEvenNumberOfTimes "abaccaba"
-- True
--
-- >>> doAllCharsAppearEvenNumberOfTimes "hthth"
-- False
doAllCharsAppearEvenNumberOfTimes :: String -> Bool
doAllCharsAppearEvenNumberOfTimes s = all even $ countLowercaseOccurrences s

-- | Count the occurrences of each lowercase alphabet character in a string.
--
-- This function creates a list of occurrence counts for each lowercase alphabet character ('a' to 'z') in the input string.
-- The function uses a Map to efficiently count the occurrences in a single pass through the string.
--
-- >>> countLowercaseOccurrences "abaccaba"
-- [4,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
--
-- >>> countLowercaseOccurrences "hthth"
-- [0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0]
countLowercaseOccurrences :: String -> [Int]
countLowercaseOccurrences s = [Map.findWithDefault 0 c m | c <- ['a'..'z']]
  where m = Map.fromListWith (+) [(c, 1) | c <- s, c `elem` ['a'..'z']]
