module Main where

-- \|
-- >>> solve "ATCODER"
-- 3
-- >>> solve "HATAGAYA"
-- 5
-- >>> solve "SHINJUKU"
-- 0
solve :: String -> Int
solve = fst . foldl update (0, 0)

update :: (Int, Int) -> Char -> (Int, Int)
update (maxLen, currentLen) c
  | isACGT c = let newLen = currentLen + 1 in (max maxLen newLen, newLen)
  | otherwise = (maxLen, 0)

isACGT :: Char -> Bool
isACGT = (`elem` "ACGT")

main :: IO ()
main = do
  s <- getLine
  print $ solve s
