module Main where

import qualified MyLib (factorial)

-- |
-- main
-- >>> main
-- 6
main :: IO ()
main = do
  print $ MyLib.factorial(3)
