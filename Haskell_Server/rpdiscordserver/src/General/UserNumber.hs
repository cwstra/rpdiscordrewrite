{-# LANGUAGE BangPatterns #-}
module General.UserNumber
(GeneralSimpleNumber(..)
, ResolveException(..)
, ParseException(..)
, GRatio(..)
, sGR
, reduce
, GeneralRealNumber(..)
, GeneralComplex(..)
, GeneralNumber(..)
, PossNumber(..)
, numShow
, genExp
, realMod
, genRealtoFloat
, gReal
, imagUnit
, getIntegral
, getIntGen
, getFloat
, getRatio
) where

import qualified Data.Complex                     as DC
import           Data.Ratio
import qualified Data.Text.Lazy                   as T
import           Math.NumberTheory.Powers.Squares

data ResolveException = ResolveException T.Text
    deriving Show

data ParseException   = ParseException T.Text Integer
    deriving Show

-- Complex constructor: a:+ b, i.e. 1 + 1j = 1 :+ 1
-- Fraction constructor a % b, i.e. 1 / 2 = 1 % 2 = 2 % 4

--General Number = Either (Either Integer Fractional) (Either Float )

data GeneralSimpleNumber = GInt !Integer | GFlo !Float
  deriving (Show, Eq)

--Show for when printing out. Left as-is for debugging
--instance Show GeneralSimpleNumber where
--  show GInt n = show n
--  show GFlo n = show n

instance Num GeneralSimpleNumber where
  a + b
    | GInt x <- a, GInt y <- b = GInt $ x + y
    | GInt x <- a, GFlo y <- b = GFlo $ fromIntegral x + y
    | GFlo x <- a, GInt y <- b = GFlo $ x + fromIntegral y
    | GFlo x <- a, GFlo y <- b = GFlo $ x + y

  a * b
    | GInt x <- a, GInt y <- b = GInt $ x * y
    | GInt x <- a, GFlo y <- b = GFlo $ fromIntegral x * y
    | GFlo x <- a, GInt y <- b = GFlo $ x * fromIntegral y
    | GFlo x <- a, GFlo y <- b = GFlo $ x * y

  negate (GInt a) = GInt (negate a)
  negate (GFlo a) = GFlo (negate a)

  abs (GInt a) = GInt (abs a)
  abs (GFlo a) = GFlo (abs a)

  signum a
    | GInt x <- a = GInt $ signum x
    | GFlo x <- a, signum x < 0.0 = GInt (-1)
    | GFlo x <- a, signum x > 0.0 = GInt 1
    | GFlo _ <- a, True           = GInt 0

  fromInteger = GInt

instance Ord GeneralSimpleNumber where
  a <= b
    | GInt x <- a, GInt y <- b = x <= y
    | GInt x <- a, GFlo y <- b = fromIntegral x <= y
    | GFlo x <- a, GInt y <- b = x <= fromIntegral y
    | GFlo x <- a, GFlo y <- b = x <= y

data GRatio = GR !Integer !Integer deriving (Show)

instance Eq GRatio where
  GR a b == GR c d = a * d == b * c

instance Num GRatio where
  GR a b + GR c d = ratReduce (a*d+b*c) (b*d)

  GR a b * GR c d = ratReduce (a*b) (c*d)

  negate (GR a b) = GR (negate a) b

  abs (GR a b) = GR (abs a) b

  signum (GR a _) = GR (signum a) 1

  fromInteger n = GR (fromInteger n) 1

instance Fractional GRatio where
  recip (GR a b) = GR b a

  fromRational n = GR (numerator n) (denominator n)

instance Real GRatio where
  toRational (GR a b) = a % b

instance RealFrac GRatio where
  properFraction (GR a b) = (fromInteger $ a `div` b, GR (a `mod` b) b)

instance Ord GRatio where
  GR a b <= GR c d = a*d <= b*c

sGR :: Integer -> Integer -> GRatio
sGR a b = ratReduce (a * signum b) (abs b)

ratReduce :: Integer -> Integer -> GRatio
ratReduce !a !b = let d = gcd a b
               in GR (quot a d) (quot b d)

data GeneralRealNumber = GSimp GeneralSimpleNumber | GRat GRatio deriving (Show, Eq)

reduce :: GRatio -> GeneralRealNumber
reduce r
  | GR a 1 <- r = GSimp (GInt a)
  | otherwise = GRat r

instance Num GeneralRealNumber where
  a + b
    | GSimp x <- a, GSimp y <- b = GSimp $ x + y
    | GSimp (GInt x) <- a, GRat (GR y z) <- b = reduce $ ratReduce (x*z+y) z
    | GSimp (GFlo x) <- a, GRat (GR y z) <- b = GSimp $ GFlo $ x+fromIntegral y/fromIntegral z
    | GRat x <- a, GRat y <- b = reduce $ x + y
    | otherwise = b + a

  a * b
    | GSimp x <- a, GSimp y <- b = GSimp $ x * y
    | GSimp (GInt x) <- a, GRat (GR y z) <- b = reduce $ ratReduce (x*y) z
    | GSimp (GFlo x) <- a, GRat (GR y z) <- b = GSimp $ GFlo $ x*fromIntegral y/fromIntegral z
    | GRat x <- a, GRat y <- b = reduce $ x * y
    | otherwise = b * a

  negate (GSimp a) = GSimp (negate a)
  negate (GRat a)  = GRat (negate a)

  abs (GSimp a) = GSimp (abs a)
  abs (GRat a)  = GRat (abs a)

  signum (GSimp a)       = GSimp (signum a)
  signum (GRat (GR a _)) = GSimp $ GInt $ signum a

  fromInteger n = GSimp (fromInteger n)

instance Fractional GeneralRealNumber where
  recip a
    |GSimp (GInt x) <- a = GRat $ sGR 1 x
    |GSimp (GFlo x) <- a = GSimp $ GFlo $ recip x
    |GRat x <- a = reduce $ recip x

  fromRational n = GRat $ sGR (numerator n) (denominator n)

instance Real GeneralRealNumber where
  toRational (GSimp (GInt x)) = x % 1
  toRational (GSimp (GFlo x)) = toRational x
  toRational (GRat r)         = toRational r

fracHelper :: (RealFrac a, Integral b) => a -> (a -> c) -> (b, c)
fracHelper inner fun = (int, fun frac)
  where
    (int, frac) = properFraction inner

instance RealFrac GeneralRealNumber where
  properFraction (GSimp (GInt x)) = (fromInteger x, GSimp $ GInt 0)
  properFraction (GSimp (GFlo x)) = fracHelper x (GSimp . GFlo)
  properFraction (GRat r)         = fracHelper r GRat

instance Ord GeneralRealNumber where
  GSimp a <= GSimp b = a <= b
  GSimp (GInt a) <= GRat (GR b c)  = a*c <= b
  GSimp (GFlo a) <= GRat (GR b c)  = a * fromInteger c <= fromInteger b
  GRat (GR b c)  <= GSimp (GInt a) = b <= a*c
  GRat (GR b c)  <= GSimp (GFlo a) = fromInteger b <= a * fromInteger c
  GRat a <= GRat b = a <= b

intPow :: GeneralRealNumber -> Integer -> GeneralRealNumber
intPow a n
  | n < 0 = intPow (recip a) (negate n)
  | n == 0 = GSimp $ GInt 1
  | otherwise = a * intPow a (n-1)

expAlgR :: GeneralRealNumber -> Integer -> GeneralRealNumber
expAlgR _ 0 = GSimp $ GInt 1
expAlgR x 1 = x
expAlgR x n
  |n < 0 = expAlgR (recip x) (negate n)
  |even n = expAlgR (x*x) (n `div` 2)
  |otherwise = x * expAlgR (x*x) (n `div` 2)

realExp :: GeneralRealNumber -> GeneralRealNumber -> GeneralRealNumber
realExp _ (GSimp (GInt 0)) = GSimp $ GInt 1
realExp a@(GSimp (GInt _)) (GSimp (GInt y)) = expAlgR a y
realExp (GSimp (GInt x)) (GSimp (GFlo y)) = GSimp $ GFlo $ fromIntegral x ** y
realExp (GSimp (GFlo x)) (GSimp (GInt y)) = GSimp $ GFlo $ x ** fromIntegral y
realExp (GSimp (GFlo x)) (GSimp (GFlo y)) = GSimp $ GFlo $ x**y
realExp r@(GRat (GR _ _)) (GSimp (GInt n)) = expAlgR r n
realExp (GRat (GR a b)) (GSimp c) = realExp (GSimp $ GFlo $ fromInteger a/fromInteger b) (GSimp c)
realExp (GSimp c) (GRat (GR a b)) = realExp (GSimp c) (GSimp $ GFlo $ fromInteger a/fromInteger b)
realExp (GRat (GR a b)) (GRat (GR c d)) = realExp (GSimp $ GFlo $ fromInteger a/fromInteger b) (GSimp $ GFlo $ fromInteger c/fromInteger d)

generalRSqrt :: GeneralRealNumber -> GeneralRealNumber
generalRSqrt (GSimp (GInt a))
  |isSquare a = GSimp $ GInt $ integerSquareRoot a
  |otherwise = GSimp $ GFlo $ sqrt $ fromIntegral a
generalRSqrt (GSimp (GFlo a)) = GSimp $ GFlo $ sqrt a
generalRSqrt (GRat (GR a b))
  |isSquare a && isSquare b = GRat $ GR (integerSquareRoot a) (integerSquareRoot b)
  |otherwise = GSimp $ GFlo $ sqrt $ fromIntegral a/fromIntegral b

genRealtoFloat :: GeneralRealNumber -> Float
genRealtoFloat (GSimp (GInt a)) = fromInteger a
genRealtoFloat (GSimp (GFlo a)) = a
genRealtoFloat (GRat (GR a b))  = fromInteger a/fromInteger b

data GeneralComplex = GC !GeneralRealNumber !GeneralRealNumber
  deriving (Show)

instance Eq GeneralComplex where
  GC a b == GC c d = (a==c) && (b==d)

realPart :: GeneralComplex -> GeneralRealNumber
realPart (GC a _) = a

imagPart :: GeneralComplex -> GeneralRealNumber
imagPart (GC _ b) = b

instance Num GeneralComplex where
  (GC a b) + (GC c d) = GC (a+c) (b+d)

  GC a b * GC c d = GC (a*c-b*d) (a*d+b*c)

  negate (GC a b) = GC (negate a) (negate b)

  abs (GC a b) = GC (generalRSqrt ((a `intPow` 2)+(b `intPow` 2))) (GSimp (GInt 0))

  signum (GC a b) = GC (a / mag) (b / mag)
    where
      mag = realPart $ abs $ GC a b

  fromInteger n = GC (GSimp (GInt n)) 0

instance Fractional GeneralComplex where
  recip (GC a b) = GC (a/((a `intPow` 2)+(b `intPow` 2))) (-b/((a `intPow` 2)+(b `intPow` 2)))

  fromRational n = GC (reduce (ratReduce (numerator n) (denominator n))) (GSimp (GInt 0))

data GeneralNumber = GReal GeneralRealNumber | GComp GeneralComplex
  deriving (Eq)

simpComplex :: GeneralComplex -> GeneralNumber
simpComplex c@(GC a b)
    | b == 0 || b == 0.0 = GReal a
    | otherwise = GComp c

instance Num GeneralNumber where
  a + b
    | GReal x <- a, GReal y <- b = GReal $ x + y
    | GReal x <- a, GComp (GC y z) <- b = simpComplex $ GC (x+y) z
    | GComp x <- a, GComp y <- b = simpComplex $ x + y
    | otherwise = b + a

  a * b
    | GReal x <- a, GReal y <- b = GReal $ x * y
    | GReal x <- a, GComp (GC y z) <- b = simpComplex $ GC (x*y) $ x*z
    | GComp x <- a, GComp y <- b = simpComplex $ x * y
    | otherwise = b * a

  negate (GReal a) = GReal (negate a)
  negate (GComp a) = GComp (negate a)

  abs (GReal a) = GReal (abs a)
  abs (GComp a) = simpComplex (abs a)

  signum (GReal a) = GReal (signum a)
  signum (GComp a) = GComp (signum a)

  fromInteger n = GReal $ GSimp $ GInt n

expAlgC :: GeneralComplex -> Integer -> GeneralNumber
expAlgC _ 0 = GReal $ GSimp $ GInt 1
expAlgC x 1 = GComp x
expAlgC x n
  |n < 0 = expAlgC (recip x) (negate n)
  |even n = expAlgC (x*x) (n `div` 2)
  |otherwise = GComp x * expAlgC (x*x) (n `div` 2)

instance Fractional GeneralNumber where
  recip a
    |GReal x <- a = GReal $ recip x
    |GComp x <- a = GComp $ recip x

  fromRational n = GReal $ fromRational n

genExp :: GeneralNumber -> GeneralNumber -> GeneralNumber
genExp (GReal n) (GReal m) = GReal $ realExp n m
genExp (GComp a) (GReal (GSimp (GInt n))) = expAlgC a n
genExp (GComp a) (GReal (GSimp (GFlo flo))) = GComp (GC (GSimp $ GFlo $ DC.realPart builtInPow) (GSimp $ GFlo $ DC.imagPart builtInPow))
  where
    buildInVer = genRealtoFloat (realPart a) DC.:+ genRealtoFloat (imagPart a)
    builtInPow = buildInVer ** (flo DC.:+ 0)
genExp a@(GComp _) (GReal r@(GRat _)) = genExp a (GReal $ GSimp $ GFlo $ genRealtoFloat r)
genExp (GComp (GC a b)) (GComp (GC c d)) = GComp $ GC (GSimp $ GFlo $ DC.realPart passed) $ GSimp $ GFlo $ DC.imagPart passed
  where
    passed = (genRealtoFloat a DC.:+ genRealtoFloat b) ** (genRealtoFloat c DC.:+ genRealtoFloat d)
genExp (GReal n) j = genExp (GComp (GC n 0)) j

realMod :: GeneralRealNumber -> GeneralRealNumber -> GeneralRealNumber
realMod a (GSimp (GInt 0)) = a
realMod a b = a - (b * f)
  where
    realfloor :: GeneralRealNumber -> Integer
    realfloor (GSimp (GInt x)) = x
    realfloor (GSimp (GFlo x)) = floor x
    realfloor (GRat (GR x y))  = floor $ genRealtoFloat $ GRat $ GR x y
    f = GSimp $ GInt $ realfloor $ a/b

realShow :: GeneralRealNumber -> String
realShow num
  |GRat (GR x y) <- num = show x ++ "/" ++ show y
  |GSimp (GInt x) <- num = show x
  |GSimp (GFlo x) <- num = show x

numShow :: GeneralNumber -> String
numShow num
  |GComp (GC (GSimp (GInt 0)) y) <- num = realShow y ++ "j"
  |GComp (GC x y) <- num = realShow x ++ "+" ++ realShow y ++ "j"
  |GReal x <- num = realShow x

instance Show GeneralNumber where
  show = numShow

data PossNumber = Kept GeneralNumber
                | Dropped GeneralNumber
  deriving (Eq)

instance Show PossNumber where
  show (Kept n)    = show n
  show (Dropped n) = "//" ++ show n ++ "//"

gReal :: (Integral a) => a -> GeneralNumber
gReal n = GReal $ GSimp $ GInt $ fromIntegral n

imagUnit = GComp $ GC (GSimp $ GInt 0) (GSimp $ GInt 1)

getIntegral :: (Integral a) => GeneralNumber -> Maybe a
getIntegral (GReal (GSimp (GInt n))) = Just $ fromInteger n
getIntegral _                        = Nothing

getIntGen :: (Integer -> a) -> GeneralNumber -> Maybe a
getIntGen p (GReal (GSimp (GInt n))) = Just $ p n
getIntGen _ _                        = Nothing

getFloat :: GeneralNumber -> Maybe Float
getFloat (GReal (GSimp (GFlo f))) = Just f
getFloat _                        = Nothing

getRatio :: GeneralNumber -> Maybe Rational
getRatio (GReal (GRat (GR p q))) = Just $ p % q
getRatio _                       = Nothing
