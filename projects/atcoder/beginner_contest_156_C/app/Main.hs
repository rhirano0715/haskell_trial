import Data.List (sort)

-- Function to calculate stamina spent
staminaSpent :: [Int] -> Int -> Int
staminaSpent xs p = sum $ map (\x -> (x - p) ^ 2) xs

-- Function to find the arithmetic mean and round it to the nearest integer
mean :: [Int] -> Int
mean xs = round $ (fromIntegral $ sum xs) / (fromIntegral $ length xs)

-- Main function
minimumStamina :: [Int] -> Int
minimumStamina xs = staminaSpent xs (mean xs)

-- Function to read input and output the result
main :: IO ()
main = do
  n <- readLn :: IO Int
  xs <- fmap (map read . words) getLine
  print $ minimumStamina xs
