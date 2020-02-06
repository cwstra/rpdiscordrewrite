{-# LANGUAGE OverloadedStrings #-}
module NewDiceParser.Functions
( Assoc(..)
, FType(..)
, ResOrder(..)
, RollOrder(..)
, Action
, IgnorantFunction(..)
, igFunctionList
) where

import           Control.Monad
import qualified Control.Monad.Fail as Fail
import           Data.Maybe
import           Data.List

import qualified Data.Text.Lazy as T

import           General.UserNumber
import           NewDiceParser.Data
import           NewDiceParser.Getters

data Assoc = LeftAssoc
           | RightAssoc
           | NoneAssoc
  deriving (Show, Eq)

instance Functor Res where
  fmap f (Succeeded v) = Succeeded $ f v
  fmap _ (Failed t)    = Failed t
  fmap _ (ToDo v)      = ToDo v

instance Applicative Res where
  pure v = Succeeded v

  Succeeded f <*> Succeeded v  = Succeeded $ f v
  Succeeded _ <*> (Failed t)   = Failed t
  Succeeded _ <*> (ToDo v)     = ToDo v
  (Failed t)  <*> _            = Failed t
  (ToDo v)    <*> _            = ToDo v

instance Monad Res where
  return v = pure v

  Succeeded v  >>= f = f v
  (Failed t) >>= _ = Failed t
  (ToDo v)   >>= _ = ToDo v

instance Fail.MonadFail Res where
  fail s = Failed $ T.pack s

data FType = FInfix Float Assoc
           | FPrefix Float
           | FPostfix Float
           | FFunct

data ResOrder = ResolveNone
              | ResolveAll
              | ResolvePos [Int]

data RollOrder = RollNone
               | RollAll
               | RollPos [Int]

data IgnorantFunction = IgnorantFunction
    { igText   :: T.Text
    , igRes    :: ResOrder
    , igRoll   :: RollOrder
    , igType   :: [FType]
    , igAction :: Action
    }

fullRollFunction :: T.Text -> [FType] -> Action -> IgnorantFunction
fullRollFunction txt typ a = IgnorantFunction txt ResolveAll RollAll typ a

leftRollFunction :: T.Text -> [FType] -> Action -> IgnorantFunction
leftRollFunction txt typ a = IgnorantFunction txt ResolveAll (RollPos [0]) typ a

rightRollFunction :: T.Text -> [FType] -> Action -> IgnorantFunction
rightRollFunction txt typ a = IgnorantFunction txt ResolveAll (RollPos [1]) typ a

only1RollFunction :: T.Text -> [FType] -> Action -> IgnorantFunction
only1RollFunction txt typ a = IgnorantFunction txt (ResolvePos [0]) (RollPos [0]) typ a

noResFunction :: T.Text -> [FType] -> Action -> IgnorantFunction
noResFunction     txt typ a = IgnorantFunction txt ResolveNone RollNone typ a

diceFunction :: Action
diceFunction _ [i]
  |Just face <- getFace i = Succeeded $ Dice $ createDie 1 face
  |otherwise = Failed "Invalid face passed to die function"
diceFunction _ [i1, i2]
  |Just pool <- getInt i1, Just face <- getFace i2 = Succeeded $ Dice $ createDie pool face
  |Just pool <- getInt i1 = Failed "Invalid face passed to die function"
  |otherwise = Failed "Invalid pool size passed to die function"
diceFunction _ _ = Failed "Improper number of arguments passed to die function"

fudgeFunction :: Action
fudgeFunction _ [i]
  |Just pool <- getInt i = Succeeded $ Dice $ createDie 1 $ Right [-1, 0, 1]
  |otherwise = Failed "Invalid pool size passed to fudge die function"
fudgeFunction _ _ = Failed "Incorrect number of arguments passed to fudge die function"

kdFunction :: (Int -> KeepDrop) -> Action
kdFunction kd _ [i1, i2]
  |Dice d <- i1, Just n<- getInt i2 = Succeeded $ Dice $ d {kdTest = kd n}
  |Dice d <- i1                     = Failed "Non-integer passed to right of keep/drop function"
  |otherwise                       = Failed "Non-die passed to left of keep/drop function"
kdFunction _ _ _ = Failed "Improper number of arguments passed to keep/drop function"

rr :: DiceTest -> Dice -> Dice
rr t d = d {rrTest = t}

ex :: DiceTest -> Dice -> Dice
ex t d = d {exTest = t}

sc :: DiceTest -> Dice -> Dice
sc t d = d {scTest = t}

testEq :: (DiceTest -> Dice -> Dice) -> Action
testEq f _ [i1, i2]
  |Dice d <- i1, Number n <- i2 = Succeeded $ Dice $ f (diceEq [n]) d
  |Dice d <- i1 = Failed "Non-number passed to right of dice-testing function"
  |otherwise = Failed "Non-die passed to left of dice-testing function"
testEq _ _ _ = Failed "Improper number of arguments passed to dice-testing function"

testNeq :: (DiceTest -> Dice -> Dice) -> Action
testNeq f _ [i1, i2]
  |Dice d <- i1, Number n <- i2 = Succeeded $ Dice $ f (diceNeq [n]) d
  |Dice d <- i1 = Failed "Non-number value passed to right of dice-testing function"
  |otherwise = Failed "Non-die passed to left of dice-testing function"
testNeq _ _ _ = Failed "Improper number of arguments passed to dice-testing function"

testOrd1 :: (DiceTest -> Dice -> Dice) -> (GeneralRealNumber -> DiceTest) -> Action
testOrd1 f c _ [i1, i2]
  |Dice d <- i1, Just n <- getReal i2 = Succeeded $ Dice $ f (c n) d
  |Dice d <- i1 = Failed "Non-real argument passed to right of ordered dice-testing function"
  |otherwise = Failed "Non-die passed to left of dice-testing function"
testOrd1 _ _ _ _ = Failed "Improper number of arguments passed to dice-testing function"

testOrd2 :: (DiceTest -> Dice -> Dice) -> (GeneralRealNumber -> GeneralRealNumber -> DiceTest) -> Action
testOrd2 f c _ [i1, i2]
  |Dice d <- i1, Just [n, m] <- getRealVec i2 = Succeeded $ Dice $ f (c n m) d
  |Dice d <- i1, Just _ <- getRealVec i2 = Failed "Vector of wrong length passed to right of range dice-testing function"
  |Dice d <- i1, Vector _ _ <- i2 = Failed "Non-real vector passed to right of range dice-testing function"
  |Dice d <- i1 = Failed "Non-vector passed to right of range dice-testing function"
  |otherwise = Failed "Non-die passed to left of dice-testing function"
testOrd2 _ _ _ _ = Failed "Improper number of arguments passed to dice-testing function"

exWrapper :: Action
exWrapper dis [i1]
  |Dice d <- i1, Left n <- face d = testEq ex dis [i1, Number $ gReal n]
  |Dice d <- i1 = Failed "Die with non-numeric face passed to postfix !"
  |Just n <- getInt i1, n > 0 = Succeeded $ Number $ fromIntegral $ foldl' (*) 1 [1..n]
  |Just n <- getInt i1, n == 0 = Succeeded $ Number 1
  |Just n <- getInt i1 = Failed "Negative Integer passed to postfix !"
  |otherwise = Failed "Non-integer, non-die passed to postfix !"
exWrapper dis [i1, i2]
  |Dice d <- i1 = testEq ex dis [i1, i2]
  |otherwise = Failed "Non-die passd to infix !"
exWrapper _ _ = Failed "Improper number of arguments passed to !"

eqFun :: Action
eqFun d l
  |[i1, i2] <- l, Dice _ <- i1 = testEq sc d l
  |[] <- l = Succeeded $ Bool True
  |[_] <- l = Succeeded $ Bool True
  |h:r <- l = Succeeded $ Bool $ isJust $ foldl' eqStep (Just h) r
  where
    eqStep :: Maybe Value -> Value -> Maybe Value
    eqStep (Just v) w
      |v == w = Just v
      |otherwise = Nothing
    eqStep Nothing _ = Nothing

neqFun :: Action
neqFun d l
  |[i1, i2] <- l, Dice _ <- i1 = testNeq sc d l
  |otherwise = Succeeded $ Bool $ recNeq l
  where
    recNeq :: [Value] -> Bool
    recNeq [] = True
    recNeq (e:es)
      |e `elem` es = False
      |otherwise   = recNeq es

compWrapper :: Action -> (GeneralRealNumber -> GeneralRealNumber -> Bool) -> Action
compWrapper a c d l
  |[i1, i2] <- l, Dice _ <- i1 = a d l
  |Just r <- getRealVec (Vector d l) = Succeeded $ Bool $ recComp r
  |otherwise = Failed "Non-real arguments passed to ordered comparator"
  where
    recComp :: [GeneralRealNumber] -> Bool
    recComp [] = True
    recComp [_] = True
    recComp (m:(n:ns))
      |c m n = recComp (n:ns)
      |otherwise = False

rangeWrapper :: Action -> (GeneralRealNumber -> GeneralRealNumber -> GeneralRealNumber -> Bool) -> Action
rangeWrapper a c d [i1, i2]
  |Dice _ <- i1 = a d [i1, i2]
  |Just r <- getReal i1, Just [a, b] <- getRealVec i2 = Succeeded $ Bool $ c a b r
  |Just r <- getReal i1, Just _ <- getRealVec i2 = Failed "Vector of improper length passed to right of range function"
  |Just r <- getReal i1 = Failed "Non-real vector passed to right of range function"
  |otherwise = Failed "Non-real passed to left of range function"

toBool :: Action
toBool _ [v]
  |isTruthy v = Succeeded $ Bool True
  |otherwise  = Succeeded $ Bool False
toBool _ _ = Failed "Improper number of arguments passed to bool"

notFunction :: Action
notFunction _ [v]
  |isTruthy v = Succeeded $ Bool False
  |otherwise  = Succeeded $ Bool True
notFunction _ _ = Failed "Improper number of arguments passed to not"

andFunction :: Action
andFunction _ = Succeeded . Bool . (all isTruthy)

orFunction :: Action
orFunction  _ = Succeeded . Bool . (any isTruthy)

ifFunction :: Action
ifFunction _ [p, t, f]
  |isTruthy p = ToDo t
  |otherwise  = ToDo f
ifFunction _ _ = Failed "Improper number of arguments passed to if"

inNumeric :: GeneralNumber -> Numeric -> Bool
inNumeric n (Scalar m) = n == m
inNumeric n (NVector _ v) = any (inNumeric n) v

mapNumeric :: (GeneralNumber -> GeneralNumber) -> Numeric -> Numeric
mapNumeric f (Scalar n)  = Scalar $ f n
mapNumeric f (NVector d v) = NVector d $ map (mapNumeric f) v

zipNumeric :: (GeneralNumber -> GeneralNumber -> GeneralNumber) -> Numeric -> Numeric -> Res Numeric
zipNumeric f v1 v2
  |sizeMatch v1 v2 = Succeeded $ numericZip v1 v2
  |otherwise = Failed "Numeric function applied to vectors of differing sizes"
  where
    sizeMatch :: Numeric -> Numeric -> Bool
    sizeMatch (Scalar _)   (Scalar _)       = True
    sizeMatch (NVector _ v1) (NVector _ v2) = and $ zipWith sizeMatch v1 v2
    sizeMatch _            _                = False
    numericZip :: Numeric -> Numeric -> Numeric
    numericZip (Scalar m)   (Scalar n)       = Scalar $ f m n
    numericZip (NVector d v1) (NVector _ v2) = NVector d $ zipWith numericZip v1 v2

lumpNumeric :: (GeneralNumber -> GeneralNumber -> GeneralNumber) -> Numeric -> Numeric -> Res Numeric
lumpNumeric f (Scalar m) (Scalar n) = Succeeded $ Scalar $ f m n
lumpNumeric f (Scalar n) v          = Succeeded $ mapNumeric (n `f`) v
lumpNumeric f v          (Scalar n) = Succeeded $ mapNumeric (`f` n) v
lumpNumeric f v          w          = zipNumeric f v w

finishNumeric = liftM numericToValue

inIntegral :: Integer -> IntegralValue -> Bool
inIntegral n (IScalar m) = n == m
inIntegral n (IVector _ v) = any (inIntegral n) v

mapIntegral :: (Integer -> Integer) -> IntegralValue -> IntegralValue
mapIntegral f (IScalar n)  = IScalar $ f n
mapIntegral f (IVector d v) = IVector d $ map (mapIntegral f) v

zipIntegral :: (Integer -> Integer -> Integer) -> IntegralValue -> IntegralValue -> Res IntegralValue
zipIntegral f v1 v2
  |sizeMatch v1 v2 = Succeeded $ numericZip v1 v2
  |otherwise = Failed "Numeric function applied to vectors of differing sizes"
  where
    sizeMatch :: IntegralValue -> IntegralValue -> Bool
    sizeMatch (IScalar _)    (IScalar _)    = True
    sizeMatch (IVector _ v1) (IVector _ v2) = and $ zipWith sizeMatch v1 v2
    sizeMatch _              _              = False
    numericZip :: IntegralValue -> IntegralValue -> IntegralValue
    numericZip (IScalar m)  (IScalar n)      = IScalar $ f m n
    numericZip (IVector d v1) (IVector _ v2) = IVector d $ zipWith numericZip v1 v2

lumpIntegral :: (Integer -> Integer -> Integer) -> IntegralValue -> IntegralValue -> Res IntegralValue
lumpIntegral f (IScalar m) (IScalar n) = Succeeded $ IScalar $ f m n
lumpIntegral f (IScalar n) v           = Succeeded $ mapIntegral (n `f`) v
lumpIntegral f v           (IScalar n) = Succeeded $ mapIntegral (`f` n) v
lumpIntegral f v           w           = zipIntegral f v w

finishIntegral = liftM integralToValue

expFunction :: Action
expFunction _ [i1, i2]
  |Just m <- getNumeric i1, Just n <- getNumeric i2 = finishNumeric $ lumpNumeric genExp m n
  |otherwise = Failed "Non-numeric value passed to exponentiation"
expFunction _ _ = Failed "Improper number of arguments passed to exponentiation"

multFunction :: Action
multFunction d l
  |Just (NVector _ m) <- getNumeric (Vector d l) = finishNumeric $ foldM multStep (Scalar 1) m
  |otherwise = Failed "Non-numeric value passed to multiplication"
  where
    multStep = lumpNumeric (*)

divFunction :: Action
divFunction _ [] = Succeeded $ Number 1
divFunction d l
  |Just (NVector _ (v:vs)) <- getNumeric (Vector d l) = den vs >>= finisher v
  |otherwise = Failed "Non-numeric value passed to division"
  where
    zeroCheck :: Res Numeric -> Res Numeric
    zeroCheck s@(Succeeded n)
      |inNumeric 0 n = Failed "Division by Zero"
      |otherwise     = s
    zeroCheck r = r
    den :: [Numeric] -> Res Numeric
    den ls = zeroCheck $ foldM (lumpNumeric (*)) (Scalar 1) ls
    finisher :: Numeric -> Numeric -> Res Value
    finisher n m= finishNumeric $ lumpNumeric (/) n m

modFunction :: Action
modFunction _ [i1, i2]
  |Just m <- getIntegralValue i1, Just n <- getIntegralValue i2 = finishIntegral $ modStep m n
  |otherwise = Failed "Non-integral value passed to modulus"
  where
    modStep = lumpIntegral mod
modFunction _ _ = Failed "Improper number of arguments passed to modulus"

addFunction :: Action
addFunction d l
  |Just (NVector _ m) <- getNumeric (Vector d l) = finishNumeric $ foldM addStep (Scalar 0) m
  |otherwise = Failed "Non-numeric value passed to multiplication"
  where
    addStep = lumpNumeric (+)

subFunction :: Action
subFunction _ [] = Succeeded $ Number 0
subFunction _ [i]
  |Just n <- getNumeric i = Succeeded $ numericToValue $ mapNumeric negate n
  |otherwise = Failed "Non-numeric value passed to minus"
subFunction d l
  |Just (NVector _ (v:vs)) <- getNumeric (Vector d l) = minus vs >>= finisher v
  |otherwise = Failed "Non-numeric value passed to minus"
  where
    minus :: [Numeric] -> Res Numeric
    minus ls = foldM (lumpNumeric (+)) (Scalar 0) ls
    finisher :: Numeric -> Numeric -> Res Value
    finisher n m = finishNumeric $ lumpNumeric (-) n m

ceilFunction :: Action
ceilFunction _ [i]
  |Just n <- getReal i = Succeeded $ Number $ gReal $ ceiling n
  |otherwise = Failed "Non-number passed to ceiling"
ceilFunction d l@[_, _] = divFunction d l >>= finisher
  where
    finisher :: Value -> Res Value
    finisher r
      |Just n <- getReal r = Succeeded $ Number $ gReal $ ceiling n
ceilFunction _ _ = Failed "Improper number of arguments passed to ceiling"

floorFunction :: Action
floorFunction _ [i]
  |Just n <- getReal i = Succeeded $ Number $ gReal $ floor n
  |otherwise = Failed "Non-number passed to floor"
floorFunction d l@[_, _] = divFunction d l >>= finisher
  where
    finisher :: Value -> Res Value
    finisher r
      |Just n <- getReal r = Succeeded $ Number $ gReal $ floor n
floorFunction _ _ = Failed "Improper number of arguments passed to floor"

roundFunction :: Action
roundFunction _ [i]
  |Just n <- getReal i = Succeeded $ Number $ gReal $ round n
  |otherwise = Failed "Non-number passed to round"
roundFunction d l@[_, _] = divFunction d l >>= finisher
  where
    finisher :: Value -> Res Value
    finisher r
      |Just n <- getReal r = Succeeded $ Number $ gReal $ round n
roundFunction _ _ = Failed "Improper number of arguments passed to round"

maxFunction :: Action
maxFunction _ [] = Failed "Max called with no arguments"
maxFunction d l
  |Just v <- getRealVec (Vector d l) = Succeeded $ Number $ GReal $ foldl1' max v
  |otherwise = Failed "Non-number passed to max"

minFunction :: Action
minFunction _ [] = Failed "min called with no arguments"
minFunction d l
  |Just v <- getRealVec (Vector d l) = Succeeded $ Number $ GReal $ foldl1' min v
  |otherwise = Failed "Non-number passed to min"

resId :: Action
resId f l = Succeeded $ Vector f l

igFunctionList =
  [ fullRollFunction  "d"     [FPrefix 12.5, FInfix 12 LeftAssoc] diceFunction
  , fullRollFunction  "dF"    [FPostfix 11]                       fudgeFunction
  , rightRollFunction "kh"    [FInfix 10 NoneAssoc]              $ kdFunction KeepHigh
  , rightRollFunction "kl"    [FInfix 10 NoneAssoc]              $ kdFunction KeepLow
  , rightRollFunction "dh"    [FInfix 10 NoneAssoc]              $ kdFunction DropHigh
  , rightRollFunction "dl"    [FInfix 10 NoneAssoc]              $ kdFunction DropLow
  , rightRollFunction "r"     [FInfix 10 NoneAssoc]              $ testEq rr
  , rightRollFunction "r/"    [FInfix 10 NoneAssoc]              $ testNeq rr
  , rightRollFunction "r<"    [FInfix 10 NoneAssoc]              $ testOrd1 rr diceLess
  , rightRollFunction "r<="   [FInfix 10 NoneAssoc]              $ testOrd1 rr diceLeq
  , rightRollFunction "r>"    [FInfix 10 NoneAssoc]              $ testOrd1 rr diceGreater
  , rightRollFunction "r>="   [FInfix 10 NoneAssoc]              $ testOrd1 rr diceGeq
  , rightRollFunction "rIn"   [FInfix 10 NoneAssoc]              $ testOrd2 rr diceIn
  , rightRollFunction "rOut"  [FInfix 10 NoneAssoc]              $ testOrd2 rr diceOut
  , rightRollFunction "!"     [FPostfix 10.5, FInfix 10 NoneAssoc] exWrapper
  , rightRollFunction "!/"    [FInfix 10 NoneAssoc]              $ testNeq ex
  , rightRollFunction "!<"    [FInfix 10 NoneAssoc]              $ testOrd1 ex diceLess
  , rightRollFunction "!<="   [FInfix 10 NoneAssoc]              $ testOrd1 ex diceLeq
  , rightRollFunction "!>"    [FInfix 10 NoneAssoc]              $ testOrd1 ex diceGreater
  , rightRollFunction "!>="   [FInfix 10 NoneAssoc]              $ testOrd1 ex diceGeq
  , rightRollFunction "!In"   [FInfix 10 NoneAssoc]              $ testOrd2 ex diceIn
  , rightRollFunction "!Out"  [FInfix 10 NoneAssoc]              $ testOrd2 ex diceOut
  , rightRollFunction "=="    [FInfix 1 LeftAssoc]                 eqFun
  , rightRollFunction "/="    [FInfix 1 LeftAssoc]                 neqFun
  , rightRollFunction "<"     [FInfix 1 LeftAssoc]               $ compWrapper (testOrd1 sc diceLess) (<)
  , rightRollFunction "<="    [FInfix 1 LeftAssoc]               $ compWrapper (testOrd1 sc diceLess) (<=)
  , rightRollFunction ">"     [FInfix 1 LeftAssoc]               $ compWrapper (testOrd1 sc diceLess) (>)
  , rightRollFunction ">"     [FInfix 1 LeftAssoc]               $ compWrapper (testOrd1 sc diceLess) (>)
  , rightRollFunction "In"    [FInfix 1 LeftAssoc]               $ rangeWrapper (testOrd2 sc diceIn) inComp
  , rightRollFunction "Out"   [FInfix 1 LeftAssoc]               $ rangeWrapper (testOrd2 sc diceOut) outComp
  , fullRollFunction  "bool"  [FFunct]                             toBool
  , fullRollFunction  "~"     [FPrefix 6]                          notFunction
  , fullRollFunction  "not"   [FFunct]                             notFunction
  , fullRollFunction  "&&"    [FInfix 3 LeftAssoc]                 andFunction
  , fullRollFunction  "and"   [FInfix 3 LeftAssoc]                 andFunction
  , fullRollFunction  "||"    [FInfix 2 LeftAssoc]                 orFunction
  , fullRollFunction  "or"    [FInfix 2 LeftAssoc]                 orFunction
  , only1RollFunction "if"    [FFunct]                             ifFunction
  , fullRollFunction  "^"     [FInfix 4 RightAssoc]                expFunction
  , fullRollFunction  "**"    [FInfix 4 RightAssoc]                expFunction
  , fullRollFunction  "*"     [FInfix 3 LeftAssoc]                 multFunction
  , fullRollFunction  "/"     [FInfix 3 LeftAssoc]                 divFunction
  , fullRollFunction  "%"     [FInfix 3 LeftAssoc]                 modFunction
  , fullRollFunction  "+"     [FInfix 2 LeftAssoc]                 addFunction
  , fullRollFunction  "-"     [FPrefix 2.5, FInfix 2 LeftAssoc]    subFunction
  , fullRollFunction  "sum"   [FFunct]                             addFunction
  , fullRollFunction  "prod"  [FFunct]                             multFunction
  , fullRollFunction  "ceil"  [FFunct]                             ceilFunction
  , fullRollFunction  "floor" [FFunct]                             floorFunction
  , fullRollFunction  "round" [FFunct]                             roundFunction
  , fullRollFunction  "max"   [FFunct]                             maxFunction
  , fullRollFunction  "min"   [FFunct]                             minFunction
  , noResFunction     "quote" [FFunct]                             resId
  , fullRollFunction  "roll"  [FFunct]                             resId
  ]
