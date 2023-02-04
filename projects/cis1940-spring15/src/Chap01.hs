module Chap01 (intMax, intMin, lengthReallyBigInteger, sampleDouble, valueTrue, valueFalse) where

-- |
-- intMax
-- >>> intMax
-- 9223372036854775807
intMax :: Int
intMax = maxBound

-- |
-- intMin
-- >>> intMin
-- -9223372036854775808
intMin :: Int
intMin = minBound

reallyBigInteger :: Integer -> Integer
reallyBigInteger n = n ^ ( n ^ ( n ^ ( n ^ n ) ) )

lengthReallyBigInteger :: Integer -> Int
lengthReallyBigInteger n = length(show (reallyBigInteger n))

sampleDouble :: Double
sampleDouble = 4.5387e-4

valueTrue :: Bool
valueTrue = True

valueFalse :: Bool
valueFalse = False
