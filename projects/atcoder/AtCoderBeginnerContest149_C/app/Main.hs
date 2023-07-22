-- \|
-- Module      : Main
-- Description : Find the smallest prime number that is greater than or equal to the input.
module Main where

-- | The 'isPrime' function checks whether a number is prime.
-- It takes an integer and returns 'True' if the number is prime, and 'False' otherwise.
-- Numbers less than 2 are not prime.
-- For numbers 2 and above, the function checks whether the number is divisible by all integers from 2 up to the square root of the number.
--
-- >>> isPrime 1
-- False
-- >>> isPrime 2
-- True
-- >>> isPrime 13
-- True
-- >>> isPrime 15
-- False
isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = all (\x -> n `mod` x /= 0) $ takeWhile (\x -> x * x <= n) [2 ..]

-- | The 'findPrime' function finds the smallest prime number that is greater than or equal to the input.
-- It uses the 'isPrime' function to check each number starting from the input.
-- If a prime is found, the function returns that number.
-- Otherwise, it recursively calls itself with the input incremented by one.
--
-- >>> findPrime 1
-- 2
-- >>> findPrime 2
-- 2
-- >>> findPrime 10
-- 11
-- >>> findPrime 17
-- 17
-- >>> findPrime 20
-- 23
-- >>> findPrime 99992
-- 100003
findPrime :: Int -> Int
findPrime x
  | isPrime x = x
  | otherwise = findPrime (x + 1)

-- | The 'main' function reads an integer from the input,
-- finds the smallest prime number that is greater than or equal to the input using 'findPrime',
-- and then prints that number.
main :: IO ()
main = do
  x <- readLn :: IO Int
  print $ findPrime x
