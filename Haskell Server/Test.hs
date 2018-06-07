module Test(smartIntegerToInt) where

smartIntegerToInt :: Integer -> Int
smartIntegerToInt n
  |n > fromIntegral (maxBound::Int) = maxBound::Int
  |n < fromIntegral (minBound::Int) = minBound::Int
  |otherwise = fromInteger n
