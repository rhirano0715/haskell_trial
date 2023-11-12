-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc071/tasks/abc071_b
module Main where

-- |
-- >>> solve "atcoderregularcontest"
-- "b"
-- >>> solve "abcdefghijklmnopqrstuvwxyz"
-- "None"
-- >>> solve "fajsonlslfepbjtsaayxbymeskptcumtwrmkkinjxnnucagfrg"
-- "d"
solve :: String -> String
solve s
  | null unused = "None"
  | otherwise = [head unused]
  where
    alphabet = ['a'..'z']
    unused = [c | c <- alphabet, notElem c s]

main :: IO ()
main = do
  s <- getLine

  putStrLn $ solve s
