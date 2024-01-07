-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/agc002/tasks/agc002_a
module Main where


main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine

  putStrLn $ solve a b

-- | Solve the problem
--
-- >>> solve 1 3
-- "Positive"
--
-- >>> solve (-3) (-1)
-- "Negative"
--
-- >>> solve (-1) 1
-- "Zero"
solve :: Int -> Int -> String
solve a b
  | a <= 0 && b >= 0 = "Zero"
  | a > 0 = "Positive"
  | odd (b - a + 1) = "Negative"
  | otherwise = "Positive"
