module Main where

-- |
-- >>> solve 4 5
-- 10
-- >>> solve 7 3
-- 11
-- >>> solve 1000000000 1000000000
-- 500000000000000000
-- >>> solve 1 1
-- 1
-- >>> solve 1 2
-- 1
-- >>> solve 1 3
-- 1
-- >>> solve 1 4
-- 1
-- >>> solve 1 5
-- 1
-- >>> solve 5 1
-- 1
-- >>> solve 4 1
-- 1
solve :: Int -> Int -> Int
solve h w
  | h == 1 = 1
  | w == 1 = 1
  | dived * 2 == hw = dived
  | otherwise = dived + 1
  where
    hw = h * w
    dived = hw `div` 2

main :: IO ()
main = do
  [h, w] <- map read . words <$> getLine :: IO [Int]
  print $ solve h w
