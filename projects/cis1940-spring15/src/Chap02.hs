module Chap02 where
-- module Chap02 (
--     -- Sample of Additional Syntax
--     strLength, frob,
--     sumTo20,
--     -- Parametric polymorphism
--     notEmpty,
--     doStuff1, doStuff2,
--     -- Exercise 1
--     exactMatches, colors
--   ) where

import Data.List

-- |
-- strLength Sample of local variables using let
-- >>> strLength ""
-- 0
-- >>> strLength "abc"
-- 3
strLength :: String -> Int
strLength []     = 0
strLength (_:xs) = let len_rest = strLength xs in -- in is required.
                   len_rest + 1

-- |
-- from Sample of define a local variable scoped over multiple guarded branches.
-- >>> frob ""
-- 'a'
-- >>> frob "abcdef"
-- 'x'
-- >>> frob "ab"
-- 'y'
-- >>> frob "abcd"
-- 'z'
frob :: String -> Char
frob []  = 'a'   -- len is NOT in scope here
frob str
  | len > 5   = 'x'
  | len < 3   = 'y'
  | otherwise = 'z'
  where
    len = strLength str

-- |
-- sumTo20 Sample of Accumulators
-- >>> sumTo20 [4,9,10,2,8]
-- 23
sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums   -- the acc. starts at 0
  where go :: Int -> [Int] -> Int
        go acc [] = acc   -- empty list: return the accumulated sum
        go acc (x:xs)
         | acc >= 20 = acc
         | otherwise = go (acc + x) xs

-- |
-- notEmpty Sample of Parametric polymorphism
-- >>> notEmpty []
-- False
-- >>> notEmpty [1]
-- True
-- >>> notEmpty ["a", "b"]
-- True
notEmpty :: [a] -> Bool
notEmpty (_:_) = True
notEmpty []    = False

-- Replacing partial functions

-- Often partial functions like head, tail, and so on can be replaced by pattern-matching. Consider the following two definitions:

doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs)) 

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2
-- These functions compute exactly the same result, and they are both total. But only the second one is obviously total, and it is much easier to read anyway.

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- |
-- exactMatches
-- >>> exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red]
-- 0
-- >>> exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange]
-- 2
-- >>> exactMatches [Red, Blue, Green, Yellow] [Red, Blue, Green, Yellow]
-- 4
exactMatches :: Code -> Code -> Int
exactMatches xs ys = length $ filter(uncurry (==)) $ zip xs ys


-- |
-- countColors
-- >>> countColors [Red, Blue, Yellow, Purple]
-- [1,0,1,1,0,1]
-- >>> countColors  [Green, Blue, Green, Orange]
-- [0,2,1,0,1,0]
countColors :: Code -> [Int]
countColors code = map count colors
  where
    count color = length $ filter (== color) code

-- |
-- matches
-- >>> matches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue]
-- 3
matches :: Code -> Code -> Int
matches secret guess = sum $ map (uncurry min) counts
  where
    counts = zip (countColors secret) (countColors guess)

uncurryWork :: Code -> Code -> [(Peg,Peg)]
uncurryWork xs ys = filter( uncurry (==) ) $ zip xs ys

-- |
-- getMove
-- >>>  getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue]
-- Move [Red,Orange,Orange,Blue] 1 2
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonExact
  where
    exact = countExactMatches secret guess
    nonExact = countNonExactMatches secret guess

countExactMatches :: Code -> Code -> Int
countExactMatches [] [] = 0
countExactMatches (x:xs) (y:ys)
  | x == y = 1 + countExactMatches xs ys
  | otherwise = countExactMatches xs ys

countNonExactMatches :: Code -> Code -> Int
countNonExactMatches secret guess = length secretMatches - countExactMatches secret guess
  where
    secretMatches = filter (`elem` guess) secret

-- |
-- Determines if a given code is consistent with a given move.
-- >>> isConsistent (Move [Red,Red,Blue,Green] 1 1) [Red,Blue,Yellow,Purple]
-- True
-- >>> isConsistent (Move [Red,Red,Blue,Green] 1 1) [Red,Blue,Red,Purple]
-- False
isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact nonExact) code = 
    exactMatches == exact && nonExactMatches == nonExact
  where
    exactMatches = countExactMatches code guess
    nonExactMatches = countNonExactMatches code guess

-- |
-- filterCodes
-- >>> filterCodes (Move [Blue,Red,Green,Yellow] 0 2) [[Red,Blue,Green,Yellow],[Blue,Green,Yellow,Red],[Red,Purple,Green,Orange],[Yellow,Yellow,Green,Blue],[Red,Red,Green,Yellow]]
-- [[Red,Purple,Green,Orange]]
-- filterCodes :: Move -> [Code] -> [Code]
-- filterCodes (Move guess exact nonExact) codes = filter (\code -> isConsistent (Move guess exact nonExact) code) codes

-- -- filterCodes :: Move -> [Code] -> [Code]
-- -- filterCodes move = filter (isConsistent move)
-- -- -- filterCodes move codes = filter (isConsistent move) codes
-- -- -- filterCodes move codes = filter (\c -> isConsistent move c) codes




-- -- |
-- --
-- allCodes :: Int -> [Code]
-- allCodes 0 = [[]]
-- allCodes n = concatMap (\c -> map (:c) colors) (allCodes (n-1))
