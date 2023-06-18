module Main where

isACGT :: Char -> Bool
isACGT c = c == 'A' || c == 'C' || c == 'G' || c == 'T'

-- |
-- >>> solve "ATCODER"
-- 3
-- >>> solve "HATAGAYA"
-- 5
-- >>> solve "SHINJUKU"
-- 0
solve :: String -> Int
solve s = maximum $ map length $ words $ map (\c -> if isACGT c then '1' else ' ') s

main :: IO ()
main = getLine >>= print . solve
