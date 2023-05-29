module Main where

import Data.List

-- | 'cardGameForTwo' calculates the difference in scores between Alice and Bob
--   when they play a card game with the given list of cards.
--   They play optimally, with Alice going first, and each player taking the card with the largest value on their turn.
--   The function returns the difference (Alice's score - Bob's score).
--
--   >>> cardGameForTwo [3, 1]
--   2
--
--   >>> cardGameForTwo [2, 7, 4]
--   5
--
--   >>> cardGameForTwo [20, 18, 2, 18]
--   18
cardGameForTwo :: [Int] -> Int
cardGameForTwo xs = aliceScore - bobScore
  where
    sortedCards = reverse $ sort xs
    aliceCards = map snd $ filter (even . fst) $ zip [0 ..] sortedCards
    bobCards = map snd $ filter (odd . fst) $ zip [0 ..] sortedCards
    aliceScore = sum aliceCards
    bobScore = sum bobCards

-- | The 'main' function reads the number of cards and the list of card values from standard input,
--   applies 'cardGameForTwo' to calculate the score difference, and prints the result to standard output.
main :: IO ()
main = do
  _ <- getLine
  line <- getLine
  let cards = map read $ words line :: [Int]
  print $ cardGameForTwo cards
