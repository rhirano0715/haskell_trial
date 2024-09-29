-- |
-- Module      : Main
-- Description : [B - Thin](https://atcoder.jp/contests/abc049/tasks/abc049_b)
module Main where

import Control.Monad (replicateM)

main :: IO ()
main = do
    -- Read the dimensions of the grid
    [h, w] <- map read . words <$> getLine
    -- Read the grid lines
    grid <- replicateM h getLine
    -- Print each line twice
    mapM_ putStrLn $ duplicate 2 grid

-- | duplicate
--
-- >>> duplicate 2 ["abc", "def"]
-- ["abc","abc","def","def"]
duplicate :: Int -> [String] -> [String]
duplicate n = concatMap (replicate n)

