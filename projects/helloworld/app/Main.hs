module Main where

import qualified MyLib (factorial)

main :: IO ()
main = do
  print $ MyLib.factorial(3)
