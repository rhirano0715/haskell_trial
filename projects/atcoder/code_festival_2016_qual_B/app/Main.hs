module Main where

-- |
-- >>> solveStep 0 0 2 3 "abccabaabb"
-- ["Yes","Yes","No","No","Yes","Yes","Yes","No","No","No"]
-- >>> solveStep 0 0 5 2 "cabbabaacaba"
-- ["No","Yes","Yes","Yes","Yes","No","Yes","Yes","No","Yes","No","No"]
-- >>> solveStep 0 0 2 2 "ccccc"
-- ["No","No","No","No","No"]
solveStep :: Int -> Int -> Int -> Int -> [Char] -> [String]
solveStep passed rank_among_international_students a b (s : ss)
  | s == 'a' && passed < (a + b) = "Yes" : solveStep (passed + 1) rank_among_international_students a b ss
  | s == 'b' && passed < (a + b) && rank_among_international_students < b = "Yes" : solveStep (passed + 1) (rank_among_international_students + 1) a b ss
  | s == 'b' = "No" : solveStep passed (rank_among_international_students + 1) a b ss
  | otherwise = "No" : solveStep passed rank_among_international_students a b ss
solveStep _ _ _ _ [] = []

main :: IO ()
main = do
  [_, a, b] <- map read . words <$> getLine :: IO [Int]
  s <- getLine
  mapM_ putStrLn (solveStep 0 0 a b s)
