module Main where

import Control.Monad

-- | The 'main' function serves as the entry point of the program.
main :: IO ()
main = do
  -- Parse user input into integers.
  cookieCounts <- parseInput
  -- Call the 'solve' function and print the result.
  print $ solve cookieCounts 0

-- | The 'parseInput' function reads a line from the standard input,
--   splits it into words, and converts each word to an integer.
parseInput :: IO [Int]
parseInput = fmap (map read . words) getLine

-- | The 'solve' function recursively solves the problem, taking the current
--   amounts of cookies and the count of performed actions as arguments.
--
--   >>> solve [4, 12, 20] 0
--   3
--
--   >>> solve [14, 14, 14] 0
--   -1
--
--   >>> solve [454, 414, 444] 0
--   1
--
--   >>> solve [1, 2, 4] 0
--   0
solve :: [Int] -> Int -> Int
solve [a, b, c] count
  -- If any of the people has an odd number of cookies, return the count of performed actions.
  | any odd [a, b, c] = count
  -- If all people have the same number of cookies, the actions will continue indefinitely, so return -1.
  | allEqual [a, b, c] = -1
  -- Otherwise, perform an action and then call 'solve' again with the new amounts of cookies and the incremented count.
  | otherwise = solve [newA, newB, newC] (count + 1)
  where
    -- Calculate the new amounts of cookies after the action.

    newA = b `div` 2 + c `div` 2
    newB = a `div` 2 + c `div` 2
    newC = a `div` 2 + b `div` 2

-- | The 'allEqual' function checks if all elements of a list are equal.
--
--   >>> allEqual [1, 1, 1]
--   True
--
--   >>> allEqual [1, 2, 3]
--   False
allEqual :: (Eq a) => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)
