module Main where

import qualified Chap01 (intMax, intMin)

-- |
-- main
-- >>> main
-- 9223372036854775807
-- -9223372036854775808
main :: IO ()
main = do
  print Chap01.intMax
  print Chap01.intMin
