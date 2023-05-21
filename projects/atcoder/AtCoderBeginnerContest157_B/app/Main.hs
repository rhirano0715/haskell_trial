module Main where

import Control.Monad (replicateM)
import Data.List (elem)

-- |
-- >>> solve [[84,97,66],[79,89,11],[61,59,7],[84,79,61],[97,89,59],[66,11,7],[84,89,7],[66,89,61]] [89,7,87,79,24,84,30]
-- True
-- >>> solve [[41,7,46],[26,89,2],[78,92,8],[41,26,78],[7,89,92],[46,2,8],[41,89,8],[46,89,78]] [6,45,16,57,17]
-- False
-- >>> solve [[60,88,34],[92,41,43],[65,73,48],[60,92,65],[88,41,73],[34,43,48],[60,41,48],[34,41,65]] [60,43,88,11,48,73,65,41,92,34]
-- True
solve :: [[Int]] -> [Int] -> Bool
solve (b : bingos) bs = isSubset b bs || solve bingos bs
solve [] _ = False

isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset [] _ = True
isSubset (x : xs) ys = x `elem` ys && isSubset xs ys

main :: IO ()
main = do
  [a11, a12, a13] <- map read . words <$> getLine :: IO [Int]
  [a21, a22, a23] <- map read . words <$> getLine :: IO [Int]
  [a31, a32, a33] <- map read . words <$> getLine :: IO [Int]
  [n] <- map read . words <$> getLine :: IO [Int]
  inputs <- replicateM n getLine
  let bs = map read inputs :: [Int]

  let bingo1 = [a11, a12, a13]
  let bingo2 = [a21, a22, a23]
  let bingo3 = [a31, a32, a33]
  let bingo4 = [a11, a21, a31]
  let bingo5 = [a12, a22, a32]
  let bingo6 = [a13, a23, a33]
  let bingo7 = [a11, a22, a33]
  let bingo8 = [a13, a22, a31]
  let bingos = [bingo1, bingo2, bingo3, bingo4, bingo5, bingo6, bingo7, bingo8]

  let result = solve bingos bs
  if result
    then putStrLn "Yes"
    else do
      putStrLn "No"
