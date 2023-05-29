module Main where

import Control.Monad

-- | The 'main' function reads the problem's inputs, calls the 'solve'
-- function with these inputs and then prints the result.
main :: IO ()
main = do
  _ <- getLine
  k <- readLn :: IO Int
  xs <- fmap (map read . words) getLine :: IO [Int]
  print $ solve k xs

-- | The 'solve' function takes the width of the plane and the list of
-- ball's positions as inputs, and returns the minimum total distance
-- the robots have to move to collect all the balls.
--
-- Each ball's position is checked by both robots, and the one with the
-- minimum distance to the ball is chosen. The total of these minimum
-- distances is returned as the result.
--
-- >>> solve 10 [2]
-- 4
--
-- >>> solve 9 [3, 6]
-- 12
--
-- >>> solve 20 [11, 12, 9, 17, 12]
-- 74
--
-- >>> solve 10 [1, 2, 3, 4, 5]
-- 30
--
-- >>> solve 20 [10, 5, 15, 2, 18]
-- 48
solve :: Int -> [Int] -> Int
solve k = sum . map (\x -> min (x * 2) ((k - x) * 2))
