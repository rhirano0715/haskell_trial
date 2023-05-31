module Main where

import Control.Monad
import Data.List

-- | The main function.
-- It reads two lines of input:
-- the first line contains two integers representing the length of the circular pond and the number of houses,
-- the second line contains a list of integers representing the positions of the houses.
-- The function then prints the minimum total distance to visit all the houses.
main :: IO ()
main = do
  -- Parse the first line of input into integers and bind them to 'k' and 'n'
  [k, n] <- map read . words <$> getLine :: IO [Int]

  -- Parse the second line of input into a list of integers and bind it to 'houses'
  houses <- map read . words <$> getLine :: IO [Int]

  -- Calculate the minimum total distance to visit all the houses and print it
  print $ minDistance k houses

-- | Function to calculate the minimum total distance to visit all the houses.
-- It takes the length of the circular pond and a list of the positions of the houses as input,
-- and returns the minimum total distance as output.
--
-- >>> minDistance 20 [5, 10, 15]
-- 10
-- >>> minDistance 20 [0, 5, 15]
-- 10
minDistance :: Int -> [Int] -> Int
minDistance k houses = k - maxDist
  where
    -- Calculate the maximum distance between adjacent houses in one pass
    maxDist = maximum $ zipWith (-) (drop 1 houses ++ [head houses + k]) houses
