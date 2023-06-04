module Main where

-- | The main function, which is the entry point of the program.
main :: IO ()
main = do
  -- Parse the inputs from the user
  inputs <- getUserInputs
  -- Calculate and print the result
  print $ calculateMinimum inputs

-- | Reads a line from the standard input, splits it into a list of words,
-- then converts each word into an Integer. It returns the two integers as a tuple.
getUserInputs :: IO (Integer, Integer)
getUserInputs = do
  line <- getLine
  let [n, k] = map read $ words line
  return (n, k)

-- | Takes a tuple of integers (n, k) and returns the minimum possible value of n
-- after performing the operation any number of times.
--
-- Examples:
--
-- >>> calculateMinimum (7, 4)
-- 1
--
-- >>> calculateMinimum (2, 6)
-- 2
--
-- >>> calculateMinimum (1000000000000000000, 1)
-- 0
calculateMinimum :: (Integer, Integer) -> Integer
calculateMinimum (n, k) = min (n `mod` k) (k - n `mod` k)
