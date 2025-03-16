-- |
-- Module      : Main
-- Description : [A - Wanna go back home](https://atcoder.jp/contests/agc003/tasks/agc003_a)
module Main where

main :: IO ()
main = do
  s <- getLine
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
solve s
  | existsNS  && existsEW  = "Yes"
  | otherwise = "No"
  where
    existsNS  = hasPair 'N' 'S' s
    existsEW  = hasPair 'E' 'W' s

-- | Check if both or neither of the two characters are in the string.
hasPair :: Char -> Char -> String -> Bool
hasPair a b s = (a `elem` s) == (b `elem` s)
