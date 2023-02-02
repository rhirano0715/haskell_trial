module MyLib (someFunc) where

-- someFunc :: IO ()
someFunc :: Int -> Int
someFunc 0 = 1
someFunc n = n * someFunc(n - 1)
