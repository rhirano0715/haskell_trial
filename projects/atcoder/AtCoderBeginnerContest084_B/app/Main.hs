-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc084/tasks/abc084_b
module Main where

-- |
-- >>> isValidPostalCode 3 4 "269-6650"
-- True
--
-- >>> isValidPostalCode 1 1 "---"
-- False
--
-- >>> isValidPostalCode 1 2 "7444"
-- False
isValidPostalCode :: Int -> Int -> String -> Bool
isValidPostalCode a b s = isValidFormat && aPlus1IsHyphen && aAndBisNumber
  where
    isValidFormat = length s == a + b + 1
    aPlus1IsHyphen = s !! a == '-'
    aAndBisNumber = all (`elem` ['0' .. '9']) (take a s ++ drop (a + 1) s)

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  s <- getLine
  putStrLn $ if isValidPostalCode a b s then "Yes" else "No"
