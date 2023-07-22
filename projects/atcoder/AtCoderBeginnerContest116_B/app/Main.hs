module Main where

import Data.Set qualified as Set

-- |
-- >>> collatz 8
-- 4
-- >>> collatz 3
-- 10
collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

-- |
-- >>> solve 8 Set.empty 1
-- 5
-- >>> solve 7 Set.empty 1
-- 18
-- >>> solve 54 Set.empty 1
-- 114
solve :: Int -> Set.Set Int -> Int -> Int
solve s visited i
  | Set.member s visited = i
  | otherwise = solve (collatz s) (Set.insert s visited) (i + 1)

main :: IO ()
main = do
  s <- readLn
  print $ solve s Set.empty 1
