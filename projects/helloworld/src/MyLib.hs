module MyLib (factorial) where

-- |
-- factorial
-- >>> factorial 0
-- 1
-- >>> factorial 3
-- 6
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
