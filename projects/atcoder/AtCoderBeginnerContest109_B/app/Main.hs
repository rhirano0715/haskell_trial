-- |
-- Module      : Main
-- Description : https://atcoder.jp/contests/abc109/tasks/abc109_b
module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  words <- replicateM n getLine
  putStrLn $ boolToYesOrNo $ shiritori words

-- |
-- >>> boolToYesOrNo True
-- "Yes"
--
-- >>> boolToYesOrNo False
-- "No"
boolToYesOrNo :: Bool -> String
boolToYesOrNo True = "Yes"
boolToYesOrNo False = "No"

-- |
-- >>> shiritori ["hoge", "english", "hoge", "enigma"]
-- False
--
-- >>> shiritori ["basic", "c", "cpp", "php", "python", "nadesico", "ocaml", "lua", "assembly"]
-- True
shiritori :: [String] -> Bool
shiritori ws = allConsecutive ws && allUnique ws

-- |
-- >>> allConsecutive ["basic", "c", "cpp"]
-- True
--
-- >>> allConsecutive ["basic", "c", "python"]
-- False
--
-- >>> allConsecutive ["python", "c", "cpp"]
-- False
--
-- >>> allConsecutive ["basic", "fsharp", "python"]
-- False
--
-- >>> allConsecutive ["basic", "c", "python"]
-- False
allConsecutive :: [String] -> Bool
allConsecutive ws = all (\(a, b) -> last a == head b) (zip ws (tail ws))

-- |
-- >>> allUnique ["hoge", "english", "hoge", "enigma"]
-- False
--
-- >>> allUnique ["basic", "c", "cpp", "php", "python", "nadesico", "ocaml", "lua", "assembly"]
-- True
allUnique :: [String] -> Bool
allUnique ws = length (nub ws) == length ws
