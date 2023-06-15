module Main where

import Data.List

-- | The 'main' function serves as the entry point of the program.
-- It performs the sequence of operations required to alchemistRequireSorted the problem.
main :: IO ()
main = do
  discardIngredientCount
  ingredientValues <- getIngredientValues
  -- print $ alchemistRequireSorted $ sort ingredientValues
  print $ alchemist ingredientValues

-- | The 'discardIngredientCount' function reads the first line of the input (the number of students)
-- and discards it because it is not needed for this problem.
discardIngredientCount :: IO ()
discardIngredientCount = getLine >> return ()

-- | The 'getIngredientValues' function retrieves the entry order of the students.
-- It reads the second line of the input, which contains the entry order of students,
-- and converts it to a list of integers.
getIngredientValues :: IO [Double]
getIngredientValues = map read . words <$> getLine

-- |
-- >>> alchemistRequireSorted [3, 4]
-- 3.5
-- >>> alchemistRequireSorted [200, 300, 500]
-- 375.0
-- >>> alchemistRequireSorted [138, 138, 138, 138, 138]
-- 138.0
alchemistRequireSorted :: [Double] -> Double
alchemistRequireSorted [x] = x
alchemistRequireSorted (x : y : xs) = alchemistRequireSorted $ sort (new : xs)
  where
    new = newIngredent x y

-- |
-- >>> alchemist [3, 4]
-- 3.5
-- >>> alchemist [500, 300, 200]
-- 375.0
-- >>> alchemist [138, 138, 138, 138, 138]
-- 138.0
alchemist :: [Double] -> Double
alchemist xs = alchemistRequireSorted $ sort xs

newIngredent :: Double -> Double -> Double
newIngredent x y = (x + y) / 2
