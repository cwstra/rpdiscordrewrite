module General.UserNumberSpec where

import Data.Ratio
import Test.Hspec
import Test.QuickCheck

import Debug.Trace

import General.UserNumber

sureRat :: Integer -> Integer -> (Integer, Integer)
sureRat x y
    |x == 0, y `elem` [-1,0,1]        = (1, 2)
    |x == 0                           = (1, y)
    |y `elem` [-1,0,1]                = (x, x + signum x)
    |abs x >= abs y, gcd x y == abs y = sureRat x $ y + signum y
    |otherwise                        = (x, y)

spec :: Spec
spec = do
  describe "General.UserNumber" $ do

    describe "sGR" $ do

      it "functions as the Rational constructor (%)" $ property $
        \x y -> let GR p q = sGR x y
                    r      = x % y
                 in y == 0 || numerator r == p && denominator r == q

    describe "reduce" $ do

      it "turns integral rationals into GInts" $ property $
        \x -> let r = sGR x 1 in (GSimp $ GInt x) == reduce r

      it "does not change other rationals" $ property $
        \x y -> let (p, q) = sureRat x y
                in fromRational (p % q) == reduce (sGR p q)
