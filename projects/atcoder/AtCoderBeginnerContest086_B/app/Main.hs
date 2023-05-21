module Main where

-- |
-- >>> solve 1 21
-- True
-- >>> solve 100 100
-- False
-- >>> solve 12 10
-- False
solve :: Int -> Int -> Bool
solve a b = isInteger a_b_sqrt
  where
    a_b = show a ++ show b
    a_b_int = strToInt a_b
    a_b_sqrt = sqrtOfX a_b_int

strToInt :: String -> Int
strToInt s = read s :: Int

sqrtOfX :: Int -> Double
sqrtOfX x = sqrt (fromIntegral x)

isInteger :: Double -> Bool
isInteger x = (fromIntegral (floor x) :: Double) == x && (fromIntegral (ceiling x) :: Double) == x

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  let isInt = solve a b
  if isInt
    then putStrLn "Yes"
    else do
      putStrLn "No"
