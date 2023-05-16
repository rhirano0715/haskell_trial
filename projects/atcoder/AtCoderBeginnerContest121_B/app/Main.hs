module Main where

import Control.Monad (replicateM)
import Data.List (sort)

-- |
-- >>> let c = -10
-- >>> solve 2 3 c [1,2,3] [[3,2,1],[1,2,2]]
-- 1
-- >>> let c = -4
-- >>> solve 5 2 c [-2,5] [[100,41],[100,40],[-3,0],[-6,-2],[18,-13]]
-- 2
-- >>> let c = 0
-- >>> solve 3 3 c [100,-100,0] [[0,100,100],[100,100,100],[-100,100,100]]
-- 0
solve :: Int -> Int -> Int -> [Int] -> [[Int]] -> Int
solve n m c bs ass = length resultsGraterThan0
  where
    abs = map (zipWith (*) bs) ass
    sumed = map (\ab -> sum ab + c) abs
    resultsGraterThan0 = filter (> 0) sumed

main :: IO ()
main = do
  [n, m, c] <- map read . words <$> getLine :: IO [Int]
  bs <- map read . words <$> getLine :: IO [Int]
  ass <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
  print $ solve n m c bs ass
