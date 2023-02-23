module Chap02 (
    -- Sample of Additional Syntax
    strLength, frob,
    sumTo20,
    -- Parametric polymorphism
    notEmpty,
    doStuff1, doStuff2
  ) where

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
