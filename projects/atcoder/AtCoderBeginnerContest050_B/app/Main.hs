-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc050/tasks/abc050_b
module Main where

import Control.Monad (replicateM)

main :: IO ()
main = do
    n <- readLn :: IO Int
    t <- map read . words <$> getLine :: IO [Int]
    m <- readLn :: IO Int
    px <- replicateM m $ do
        [p, x] <- map read . words <$> getLine
        return (p, x)
    let totalTime = sum t
    mapM_ (print . updateTotalTime totalTime t) px

updateTotalTime :: Int -> [Int] -> (Int, Int) -> Int
updateTotalTime totalTime t (p, x) = totalTime - t !! (p - 1) + x
