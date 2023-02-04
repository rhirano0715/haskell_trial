module MyLib (someFunc) where

-- |
-- someFunc :: IO ()
--
-- >>> someFunc 3
-- 6
someFunc :: Int -> Int
someFunc 0 = 1
someFunc n = n * someFunc(n - 1)
