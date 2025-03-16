-- |
-- Module      : Main
-- Description : [A - Wanna go back home](https://atcoder.jp/contests/agc003/tasks/agc003_a)
module Main where

main :: IO ()
main = do
  s <- map read . words <$> getLine :: IO String
  putStrLn $ solve s

-- | solve the problem
--
-- >>> solve "SENW"
-- "Yes"
--
-- >>> solve "NSNNSNSN"
-- "Yes"
--
-- >>> solve "NNEW"
-- "No"
--
-- >>> solve "W"
-- "No"
solve :: String -> String
solve s = result
  where
    existsNouthSouthSet = ('N' `elem` s) == ('S' `elem` s)
    existsEastWestSet = ('E' `elem` s) == ('W' `elem` s)
    result = if existsNouthSouthSet && existsEastWestSet
              then "Yes"
              else "No"
