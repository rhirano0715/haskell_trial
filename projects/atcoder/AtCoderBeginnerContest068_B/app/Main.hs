module Main where

-- | The 'main' function:
--   This is the entry point for the program.
--   It reads an integer from the standard input and prints the result of the 'solve' function.
main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ solve n

-- | The 'solve' function:
--   Given an integer 'n', it returns the maximum number that is a power of 2 and is less than or equal to 'n'.
--
-- For example:
-- >>> solve 7
-- 4
-- >>> solve 32
-- 32
-- >>> solve 100
-- 64
solve :: Int -> Int
solve n = last . takeWhile (<= n) $ iterate (* 2) 1
