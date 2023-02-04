module Main where

import qualified Chap01 (intMax)

-- |
-- main
-- >>> main
-- 9223372036854775807
main :: IO ()
main = do
  print Chap01.intMax
