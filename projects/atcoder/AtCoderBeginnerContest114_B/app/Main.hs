-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc114/tasks/abc114_b
module Main where

import Control.Monad (replicateM_)
import Data.List (tails)

toInt :: String -> Int
toInt = read

diffFrom753 :: Int -> Int
diffFrom753 x = abs (753 - x)

-- |
-- >>> solve "1234567876"
-- 34
--
-- >>> solve "35753"
-- 0
--
-- >>> solve "1111111111"
-- 642
solve :: String -> Int
solve s = minimum [diffFrom753 . toInt $ take 3 t | t <- tails s, length t >= 3]

main :: IO ()
main = do
  s <- getLine
  print $ solve s
