-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc062/tasks/abc062_b
module Main where

main :: IO ()
main = do
    _ <- getLine
    contents <- getContents
    let picture = lines contents
    let framedPicture = solve picture
    putStrLn framedPicture

-- | Solve the problem
--
-- >>> solve ["abc", "def"]
-- "#####\n#abc#\n#def#\n#####\n"
--
solve :: [String] -> String
solve picture = 
    let width = length (head picture) + 2
        frame = replicate width '#'
    in unlines $ frame : ["#" ++ row ++ "#" | row <- picture] ++ [frame]
