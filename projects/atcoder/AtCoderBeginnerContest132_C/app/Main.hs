module Main where

import Data.List (sort)

-- The main function of the program
-- It reads the number of problems and their difficulties from the input,
-- and prints the number of choices for K, where K is a difficulty level such that
-- the number of problems with difficulty K or higher is the same as
-- the number of problems with difficulty lower than K.
main :: IO ()
main = do
  -- Read the number of problems and store it in 'n'
  n <- readLn :: IO Int

  -- Read the difficulties of the problems, split them by space, convert each to an integer, and store the list in 'ds'
  ds <- map read . words <$> getLine :: IO [Int]

  -- Calculate the number of choices for K and print the result
  print $ numberOfChoicesForK n ds

-- | This function calculates the number of choices for K, given the number of problems and their difficulties.
-- K is a difficulty level such that the number of problems with difficulty K or higher is the same as
-- the number of problems with difficulty lower than K.
--
-- Examples:
--
-- >>> numberOfChoicesForK 4 [1, 3, 1, 3]
-- 2
--
-- >>> numberOfChoicesForK 6 [1, 2, 3, 4, 5, 6]
-- 1
--
-- >>> numberOfChoicesForK 6 [9, 1, 4, 4, 6, 7]
-- 2
--
-- >>> numberOfChoicesForK 8 [9, 1, 14, 5, 5, 4, 4, 14]
-- 0
--
-- >>> numberOfChoicesForK 14 [99592, 10342, 29105, 78532, 83018, 11639, 92015, 77204, 30914, 21912, 34519, 80835, 100000, 1]
-- 42685
numberOfChoicesForK :: Int -> [Int] -> Int
numberOfChoicesForK n ds =
  let sorted = sort ds -- Sort the difficulties in ascending order
   in sorted !! (n `div` 2) - sorted !! (n `div` 2 - 1) -- Calculate and return the difference between the two middle elements in the sorted list
