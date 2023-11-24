-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc141/tasks/abc141_c
module Main where

import Control.Monad (replicateM)

-- |
-- >>> solve 6 3 4 [3,1,3,2]
-- ["No","No","Yes","No","No","No"]
--
-- >>> solve 6 5 4 [3,1,3,2]
-- ["Yes","Yes","Yes","Yes","Yes","Yes"]
--
-- >>> solve 10 13 15 [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9]
-- ["No","No","No","No","Yes","No","No","No","Yes","No"]
solve :: Int -> Int -> Int -> [Int] -> [String]
solve n k q answers = [pass p | p <- lastPoints]
  where
    startPoints = replicate n k
    players = [0..n-1]
    steps = [ [if x == a - 1 then 0 else -1 | x <- players] | a <- answers ]
    lastPoints = foldl (zipWith (+)) startPoints steps


pass :: Int -> String
pass point
  | point > 0 = "Yes"
  | otherwise = "No"

main :: IO ()
main = do
  [n, k, q] <- map read . words <$> getLine
  answers <- replicateM q (read <$> getLine)

  mapM_ putStrLn $ solve n k q answers
