module Main where

import qualified MyLib (someFunc)

main :: IO ()
main = do
  print $ MyLib.someFunc(3)
