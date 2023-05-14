module Main where

-- |
-- >>> solve 432
-- 400
-- >>> solve 1079
-- -1
-- >>> solve 1001
-- 927
solve :: Int -> Int
solve n = canGetXBackToN (fromIntegral n / 1.08) n

canGetXBackToN :: Float -> Int -> Int
canGetXBackToN x n
  | floor (fromIntegral (ceiling x) * 1.08) == n = ceiling x
  | floor (fromIntegral (floor x) * 1.08) == n = floor x
  | otherwise = -1

main :: IO ()
main = do
  [n] <- map read . words <$> getLine :: IO [Int]
  let result = solve n
  if result == -1
    then putStrLn ":("
    else do
      print result
