-- |
-- Module      : Main
-- Description : [B - A to Z String](https://atcoder.jp/contests/abc053/tasks/abc053_b)
module Main where
import Data.Maybe (fromJust, isNothing)

main :: IO ()
main = do
    s <- getLine
    print (solve s)

-- | solve is the function that solves the problem
-- >>> solve "QWERTYASDFZXCV"
-- 5
--
-- >>> solve "ZABCZ"
-- 4
--
-- >>> solve "HASFJGHOGAKZZFEGA"
-- 12
solve :: String -> Int
solve s = result
  where
    (Just firstA, Just lastZ) = go s 1 Nothing Nothing
    result = lastZ - firstA + 1

go :: String -> Int -> Maybe Int -> Maybe Int -> (Maybe Int, Maybe Int)
go [] _ firstA lastZ = (firstA, lastZ)
go (c:cs) i firstA lastZ
  | c == 'A' && isNothing firstA = go cs (i + 1) (Just i) lastZ
  | c == 'Z' = go cs (i + 1) firstA (Just i)
  | otherwise = go cs (i + 1) firstA lastZ
