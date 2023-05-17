module Main where

import Control.Monad (replicateM)
import Data.List (sort)

-- |
-- >>> solve 4 5
-- 10
-- >>> solve 7 3
-- 11
-- >>> solve 1000000000 1000000000
-- 500000000000000000
solve :: Int -> Int -> Int
solve h w = solveStepH hs ws
  where
    hs = makeArray h
    ws = makeArray w
    -- results = solveStepH hs ws
    -- trues = filter (\r -> r == True) results

-- |
-- >>> makeArray 4
-- [1,2,3,4]
-- >>> makeArray 1
-- [1]
makeArray :: Int -> [Int]
makeArray max
  | max <= 1 = [1]
  | otherwise = [1 .. max]

solveStepH :: [Int] -> [Int] -> Int
solveStepH (h : hs) ws = (solveStepW h ws) + (solveStepH hs ws)
solveStepH (h : []) ws = solveStepW h ws
solveStepH [] ws = 0

-- |
-- >>> solveStepW 1 [1]
-- 1
-- >>> solveStepW 1 [1,2]
-- 1
-- >>> solveStepW 1 [1,3]
-- 2
solveStepW :: Int -> [Int] -> Int
solveStepW h (w : ws) = (couldMove h w) + solveStepW h ws
solveStepW h (w : []) = (couldMove h w)
solveStepW _ [] = 0

-- |
-- >>> couldMove 1 1
-- 1
-- >>> couldMove 1 3
-- 1
-- >>> couldMove 1 2
-- 0
couldMove :: Int -> Int -> Int
couldMove h w
  | result == True = 1
  | otherwise = 0
  where
    result = mod (h + w) 2 == 0

main :: IO ()
main = do
  [h, w] <- map read . words <$> getLine :: IO [Int]
  print $ solve h w
