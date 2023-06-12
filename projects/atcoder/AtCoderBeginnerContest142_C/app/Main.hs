module Main where

import Control.Monad
import Data.List
import Data.Ord

-- | The 'main' function serves as the entry point of the program.
-- It performs the sequence of operations required to solve the problem.
main :: IO ()
main = do
  discardStudentCount
  studentEntryOrder <- getStudentEntryOrder
  let studentNumbersInEntryOrder = processStudentEntryOrder studentEntryOrder
  printStudentNumbers studentNumbersInEntryOrder

-- | The 'discardStudentCount' function reads the first line of the input (the number of students)
-- and discards it because it is not needed for this problem.
discardStudentCount :: IO ()
discardStudentCount = getLine >> return ()

-- | The 'getStudentEntryOrder' function retrieves the entry order of the students.
-- It reads the second line of the input, which contains the entry order of students,
-- and converts it to a list of integers.
getStudentEntryOrder :: IO [Int]
getStudentEntryOrder = map read . words <$> getLine

-- | The 'processStudentEntryOrder' function takes a list of integers that represent the entry order of students
-- and generates a list of student numbers based on the entry order.
-- >>> processStudentEntryOrder [2, 3, 1]
-- [3,1,2]
-- >>> processStudentEntryOrder [1, 2, 3, 4, 5]
-- [1,2,3,4,5]
-- >>> processStudentEntryOrder [8, 2, 7, 3, 4, 5, 6, 1]
-- [8,2,4,5,6,7,3,1]
processStudentEntryOrder :: [Int] -> [Int]
processStudentEntryOrder entryOrder =
  let studentNumberAndEntryOrderPairs = pairStudentNumberAndEntryOrder entryOrder
      sortedPairs = sortByEntryOrder studentNumberAndEntryOrderPairs
   in getStudentNumbers sortedPairs

-- | The 'pairStudentNumberAndEntryOrder' function takes a list of integers that represent the entry order of students
-- and pairs each entry order with the corresponding student number.
-- >>> pairStudentNumberAndEntryOrder [2, 3, 1]
-- [(2,1),(3,2),(1,3)]
pairStudentNumberAndEntryOrder :: [Int] -> [(Int, Int)]
pairStudentNumberAndEntryOrder entryOrder = zip entryOrder [1 ..]

-- | The 'sortByEntryOrder' function sorts a list of pairs of student numbers and entry orders based on the entry order.
-- >>> sortByEntryOrder [(2,1),(3,2),(1,3)]
-- [(1,3),(2,1),(3,2)]
sortByEntryOrder :: [(Int, Int)] -> [(Int, Int)]
sortByEntryOrder = sortOn fst

-- | The 'getStudentNumbers' function retrieves the student numbers from a list of pairs of student numbers and entry orders.
-- >>> getStudentNumbers [(1,3),(2,1),(3,2)]
-- [3,1,2]
getStudentNumbers :: [(Int, Int)] -> [Int]
getStudentNumbers = map snd

-- | The 'printStudentNumbers' function prints a list of student numbers.
-- It converts the student numbers to strings, concatenates them with spaces, and outputs the result.
printStudentNumbers :: [Int] -> IO ()
printStudentNumbers = putStrLn . unwords . map show
