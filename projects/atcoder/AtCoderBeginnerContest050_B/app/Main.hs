-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc050/tasks/abc050_b
module Main where

import Control.Monad (mapM)

main :: IO ()
main = do
    n <- readLn :: IO Int
    t <- map read . words <$> getLine
    m <- readLn :: IO Int
    px <- mapM (\_ -> (\[p, x] -> (p, x)) . map read . words <$> getLine) [1..m]
    let totalTime = sum t
    mapM_ (print . calculateNewTime totalTime t) px

calculateNewTime :: Int -> [Int] -> (Int, Int) -> Int
calculateNewTime totalTime t (p, x) = totalTime - t !! (p - 1) + x

-- import Control.Monad (replicateM)

-- main :: IO ()
-- main = do
--     n <- readLn :: IO Int
--     t <- map read . words <$> getLine
--     m <- readLn :: IO Int
--     px <- replicateM m parseInput
--     let totalTime = sum t
--     mapM_ (print . calculateNewTime totalTime t) px

-- parseInput :: IO (Int, Int)
-- parseInput = (\[p, x] -> (p, x)) . map read . words <$> getLine

-- calculateNewTime :: Int -> [Int] -> (Int, Int) -> Int
-- calculateNewTime totalTime t (p, x) = totalTime - t !! (p - 1) + x
