module Main where

import Text.Read (readMaybe)

main :: IO ()
main = do
  input <- getLine
  case parseInput input of
    Just (a, b) -> print $ minPowerStrips a b
    Nothing -> putStrLn "Invalid input. Please provide two integers separated by a space."

parseInput :: String -> Maybe (Int, Int)
parseInput input = do
  let wordsList = words input
  a <- readMaybe (wordsList !! 0) :: Maybe Int
  b <- readMaybe (wordsList !! 1) :: Maybe Int
  return (a, b)

-- |
-- minPowerStrips
-- >>> minPowerStrips 4 10
-- 3
-- >>> minPowerStrips 8 9
-- 2
-- >>> minPowerStrips 8 8
-- 1
minPowerStrips :: Int -> Int -> Int
minPowerStrips a b = (b - 2) `div` (a - 1) + 1
