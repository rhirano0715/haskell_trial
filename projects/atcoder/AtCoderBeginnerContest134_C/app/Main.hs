-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc134/tasks/abc134_c
module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  a <- replicateM n readLn :: IO [Int]
  let results = solve a
  mapM_ print results

-- | Solve the problem
--
-- >>> solve [1, 4, 3]
-- [4,3,4]
--
-- >>> solve [5, 5]
-- [5,5]
solve :: [Int] -> [Int]
solve a = map replaceWithMax a
  where
    maxVal = maximum a
    secondMaxVal = maximum $ delete maxVal a
    replaceWithMax x = if x == maxVal then secondMaxVal else maxVal