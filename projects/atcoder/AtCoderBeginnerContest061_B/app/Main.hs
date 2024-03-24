-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc061/tasks/abc061_b
module Main where

import qualified Data.IntMap.Strict as Map
import Control.Monad (replicateM)
import Data.List (foldl')

type Road = (Int, Int)
type Counter = Map.IntMap Int

main :: IO ()
main = do
    [n, m] <- map read . words <$> getLine
    roads <- replicateM m $ (\[a, b] -> (a, b)) . map read . words <$> getLine
    mapM_ print $ solve n roads

-- | Solve the problem.
--
-- >>> solve 4 [(1, 2), (2, 3), (1, 4)]
-- [2,2,1,1]
--
-- >>> solve 2 [(1, 2), (2, 1), (1, 2), (2, 1), (1, 2)]
-- [5,5]
--
-- >>> solve 8 [(1, 2), (3, 4), (1, 5), (2, 8), (3, 7), (5, 2), (4, 1), (6, 8)]
-- [3,3,2,2,2,1,1,2]
solve :: Int -> [Road] -> [Int]
solve n roads = map (\i -> Map.findWithDefault 0 i counter) [1..n]
    where
        counter = count roads

-- | Count the number of roads connected to each town.
count :: [Road] -> Counter
count = foldl' (\acc (a, b) -> Map.insertWith (+) b 1 $ Map.insertWith (+) a 1 acc) Map.empty
