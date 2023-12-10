-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc141/tasks/abc141_c
module Main where

import qualified Data.IntMap as Map
import Control.Monad (replicateM)

-- |
-- >>> solve 6 3 4 [3,1,3,2]
-- ["No","No","Yes","No","No","No"]
--
-- >>> solve 6 5 4 [3,1,3,2]
-- ["Yes","Yes","Yes","Yes","Yes","Yes"]
--
-- >>> solve 10 13 15 [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9]
-- ["No","No","No","No","Yes","No","No","No","Yes","No"]
solve :: Int -> Int -> Int -> [Int] -> [String]
solve n k q answers = map (result . (`Map.lookup` finalScores)) [1..n]
  where
    initialScores = Map.fromList $ zip [1..n] (repeat (k - q))
    finalScores = foldl (\acc x -> Map.adjust (+1) x acc) initialScores answers
    result (Just score) | score > 0 = "Yes"
    result _ = "No"

main :: IO ()
main = do
    [n, k, q] <- map read . words <$> getLine
    answers <- replicateM q (read <$> getLine)
    mapM_ putStrLn $ solve n k q answers
