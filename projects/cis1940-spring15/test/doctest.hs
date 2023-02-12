module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Chap01.hs", "src/Chap02.hs", "app/Main.hs"]
