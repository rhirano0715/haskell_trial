module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Chap01.hs", "app/Main.hs"]
