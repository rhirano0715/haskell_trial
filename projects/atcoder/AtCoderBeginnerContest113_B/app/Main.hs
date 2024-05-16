-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc113/tasks/abc113_b
module Main where

import Control.Monad
import Data.List
import Data.Ord

findComfortableCity :: Int -> [Double] -> [Double] -> Int
findComfortableCity n [t, a] hs = fst $ minimumBy (comparing snd) $ zip [1..] diffs
  where
    temps = map (\h -> t - h * 0.006) hs
    diffs = map (abs . subtract a) temps

main :: IO ()
main = do
  n <- readLn :: IO Int
  ta <- map read . words <$> getLine :: IO [Double]
  hs <- map read . words <$> getLine :: IO [Double]
  print $ findComfortableCity n ta hs

-- TODO: Broken:

-- import Control.Monad
-- import Data.List
-- import Data.Ord

-- -- | Find the city with the most comfortable temperature.
-- --
-- -- >>> findComfortableCity 2 [12, 7] [1000, 2000]
-- -- 1
-- --
-- -- >>> findComfortableCity 3 [21, -11] [81234, 94124, 52141]
-- -- 3
-- findComfortableCity :: Int -> [Double] -> [Double] -> Int
-- findComfortableCity n [t, a] hs = fst $ minimumBy (comparing snd) $ zip [1..] diffs
--   where
--     temps = calcurateTemperatures hs t
--     diffs = calculateTempHumidityDifferences temps a

-- -- | Calculate the temperature.
-- --
-- -- >>> calcurateTemperature 1000 12
-- -- 6.0
-- --
-- -- >>> calcurateTemperature 2000 12
-- -- 0.0
-- --
-- -- >>> calcurateTemperature 0 0
-- -- 0.0
-- --
-- -- >>> calcurateTemperature 81234 21
-- -- -466.404
-- --
-- -- >>> calcurateTemperature 94124 21
-- -- -543.744
-- --
-- -- >>> calcurateTemperature 52141 21
-- -- -291.846
-- calcurateTemperature :: Double -> Double -> Double
-- calcurateTemperature h t = t - h * 0.006

-- calcurateTemperatures :: [Double] -> Double -> [Double]
-- calcurateTemperatures hs t = map (calcurateTemperature t) hs

-- calculateTempHumidityDifferences :: [Double] -> Double -> [Double]
-- calculateTempHumidityDifferences temps a = map (abs . subtract a) temps

-- main :: IO ()
-- main = do
--   n <- readLn :: IO Int
--   ta <- map read . words <$> getLine :: IO [Double]
--   hs <- map read . words <$> getLine :: IO [Double]
--   print $ findComfortableCity n ta hs
