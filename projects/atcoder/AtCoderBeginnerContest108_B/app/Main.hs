-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc108/tasks/abc108_b
module Main where

-- |
-- >>> solve 0 0 0 1
-- [-1,1,-1,0]
--
-- >>> solve 2 3 6 6
-- [3,10,-1,7]
--
-- >>> solve 31 (-41) (-59) 26
-- [-126,-64,-36,-131]
solve :: Int -> Int -> Int -> Int -> [Int]
solve x1 y1 x2 y2 = [x3, y3, x4, y4]
  where
    (dx, dy) = (x2 - x1, y2 - y1)
    (x3, y3) = (x2 - dy, y2 + dx)
    (x4, y4) = (x1 - dy, y1 + dx)

main :: IO ()
main = do
  [x1, y1, x2, y2] <- fmap (map read . words) getLine :: IO [Int]
  let [x3, y3, x4, y4] = solve x1 y1 x2 y2
  putStrLn $ unwords [show x3, show y3, show x4, show y4]
