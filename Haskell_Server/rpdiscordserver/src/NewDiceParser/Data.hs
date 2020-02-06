{-# LANGUAGE OverloadedStrings #-}
module NewDiceParser.Data
( tShow
, (<++>)
, History
, Historical
, inComp
, outComp
, KeepDrop(..)
, DiceTest(..)
, diceTest
, diceEq
, diceNeq
, diceLess
, diceLeq
, diceGreater
, diceGeq
, diceIn
, diceOut
, Face
, Dice(..)
, createDie
, addKeepDrop
, addReroll
, addExplode
, addSuccess
, PossNumber
, Numeric(..)
, numericToValue
, IntegralValue(..)
, integralToValue
, Value(..)
, printValue
, Res(..)
, RollerStateT
, RollRes
, Action
, ResAction
, NameSpace
, isTruthy
, treePrint
, lispPrint
, inlineParenPrint
, inlineBracketPrint
) where

import           Control.Monad
import           Data.Int
import           Data.List

import           Control.Monad.State.Lazy
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text.Lazy as T
import           System.Random.Mersenne.Pure64

import           General.UserNumber

type History = [(T.Text, T.Text)]

type Historical v = (v, History)

tShow :: Show a => a -> T.Text
tShow = T.pack . show

(<++>) :: T.Text -> T.Text -> T.Text
t1 <++> t2 = T.append t1 t2

inComp :: Ord a => a -> a -> a -> Bool
inComp a b x = a <= x && x <= b

outComp :: Ord a => a -> a -> a -> Bool
outComp a b x = a >= x || x >= b

data KeepDrop = KeepHigh Int
              | KeepLow  Int
              | DropHigh Int
              | DropLow  Int
              | KDNone
  deriving Show

data DiceTest = DEquals [GeneralNumber]
              | DNeq    [GeneralNumber]
              | DReal   T.Text (GeneralRealNumber -> Bool)
              | DNone

diceTest :: DiceTest -> GeneralNumber -> Bool
diceTest (DEquals ns) n = n `elem` ns
diceTest (DNeq    ns) n = not $ n `elem` ns
diceTest (DReal _ f) n
  |GReal r <- n = f r
  |otherwise = False
diceTest DNone _ = False

diceEq :: [GeneralNumber] -> DiceTest
diceEq ns = DEquals ns
diceNeq :: [GeneralNumber] -> DiceTest
diceNeq ns = DNeq ns
singleComp :: (GeneralRealNumber -> GeneralRealNumber -> Bool) -> T.Text -> GeneralRealNumber -> DiceTest
singleComp f t n = DReal t $ f n
diceLess :: GeneralRealNumber -> DiceTest
diceLess = singleComp (<) "<"
diceLeq :: GeneralRealNumber -> DiceTest
diceLeq = singleComp (<=) "<="
diceGreater :: GeneralRealNumber -> DiceTest
diceGreater = singleComp (>) ">"
diceGeq :: GeneralRealNumber -> DiceTest
diceGeq = singleComp (>=) ">="
diceIn :: GeneralRealNumber -> GeneralRealNumber -> DiceTest
diceIn n m
  |n < m = DReal "In" $ inComp n m
  |otherwise = DReal "In" $ const False
diceOut :: GeneralRealNumber -> GeneralRealNumber -> DiceTest
diceOut n m
  |n < m     = DReal "Out" $ outComp n m
  |otherwise = DReal "Out" $ const False

type Face = Either Int [GeneralNumber]

data Dice = Die
  { pool :: Int
  , face :: Face
  , kdTest :: KeepDrop
  , rrTest :: DiceTest
  , exTest :: DiceTest
  , scTest :: DiceTest
  }

createDie :: Int -> Face -> Dice
createDie n f = Die
              { pool = n
              , face = f
              , kdTest = KDNone
              , rrTest = DNone
              , exTest = DNone
              , scTest = DNone
              }

addKeepDrop :: KeepDrop -> Dice -> Dice
addKeepDrop kd d = d {kdTest = kd}

addReroll :: DiceTest -> Dice -> Dice
addReroll t d = d {rrTest = t}

addExplode :: DiceTest -> Dice -> Dice
addExplode t d = d {exTest = t}

addSuccess :: DiceTest -> Dice -> Dice
addSuccess t d = d {scTest = t}

data Numeric = Scalar  GeneralNumber
             | NVector ([Value] -> T.Text) [Numeric]

instance Eq Numeric where
  (Scalar n)    == (Scalar m)    = n == m
  (NVector _ v) == (NVector _ w) = v == w
  _             == _             = False

numericToValue :: Numeric -> Value
numericToValue (Scalar n) = Number n
numericToValue (NVector f l) = Vector f $ map numericToValue l

data IntegralValue = IScalar Integer
                   | IVector ([Value] -> T.Text) [IntegralValue]

integralToValue :: IntegralValue -> Value
integralToValue (IScalar n) = Number $ gReal n
integralToValue (IVector f l) = Vector f $ map integralToValue l

data Value = Atom T.Text
           | String T.Text
           | Bool Bool
           | Dice Dice
           | Number GeneralNumber
           | Vector ([Value] -> T.Text) [Value]
           | Primitive T.Text ResAction
           | Function { params :: [T.Text]
                      , varArgs :: Maybe T.Text
                      , body :: Value
                      , closure :: NameSpace}

instance Show Value where
  show (Atom t)           = "Atom " ++ show t
  show (String t)         = "String " ++ show t
  show (Bool b)           = "Bool " ++ show b
  show (Number n)         = "Number " ++ show n
  show (Vector _ v)       = "Vector " ++ show v
  show (Primitive t _)    = "Primitive Function " ++ T.unpack t
  show (Function p v _ _) = "(fn (" ++ intercalate " " (map T.unpack p)
                          ++ case v of
                              Nothing -> ""
                              Just s -> T.unpack s
                          ++ ") ... )"

type RollerStateT = StateT (NameSpace, PureMT)

type RollRes      = RollerStateT Res

instance Eq Value where
  (Atom t) == (Atom s)               = t == s
  (String t) == (String s)           = t == s
  (Bool t) == (Bool s)               = t == s
  (Number m) == (Number n)           = m == n
  (Vector _ v) == (Vector _ w)       = v == w
  (Primitive t _) == (Primitive s _) = t == s
  _ == _                             = False

type NameSpace = HM.HashMap T.Text Value

data Res v = Succeeded v
           | Failed T.Text
           | ToDo Value

type Action    = ([Value] -> T.Text) -> [Value] -> Res Value
type ResAction = ([Value] -> T.Text) -> [Value] -> RollRes (Historical Value)

printValue :: Value -> T.Text
printValue (Atom t)   = t
printValue (String t) = "\"" <++> t <++> "\""
printValue (Bool b)   = T.pack $ show b
printValue (Number n) = T.pack $ show n
printValue (Vector d v) = d v
printValue (Primitive s _) = "Primitive Function " <++> s
printValue (Function p v _ _) = "(fn (" <++> T.intercalate " " p
                              <++> case v of
                                     Nothing -> ""
                                     Just s -> s
                              <++> ") ... )"

isTruthy :: Value -> Bool
isTruthy (Number 0)       = False
isTruthy (Vector _ [])    = False
isTruthy _                = True

treePrint :: T.Text -> [Value] -> T.Text
treePrint t v = T.intercalate "\n" $ t : map (printHelp 2) v
  where
    spaces n = T.replicate n " "
    printHelp :: Int64 -> Value -> T.Text
    printHelp n (Vector _ ls)    = spaces n <++> T.intercalate "\n" ("vec" : map (printHelp $ n + 2) ls)
    printHelp n val              = spaces n <++> printValue val

lispPrint :: (T.Text -> T.Text) -> [Value] -> T.Text
lispPrint f v = f $ T.intercalate " " (map printValue v)

inlineBinaryPrint :: (T.Text -> T.Text) -> [Value] -> T.Text
inlineBinaryPrint parens [f, x, y] = parens $ printValue x <++> printValue f <++> printValue y

inlinePrefixPrint :: (T.Text -> T.Text) -> [Value] -> T.Text
inlinePrefixPrint parens [f, x] = parens $ printValue f <++> printValue x

inlinePostfixPrint :: (T.Text -> T.Text) -> [Value] -> T.Text
inlinePostfixPrint parens [f, x] = parens $ printValue x <++> printValue f

inlineFunctionPrint :: (T.Text -> T.Text) -> [Value] -> T.Text
inlineFunctionPrint parens (f:l) = parens $ printValue f <++> fParens
  where
    inside = T.intercalate " ," $ map printValue l
    fParens = case l of
                [Vector _ _]    -> inside
                _               -> "(" <++> inside <++> ")"

inlineParenPrint :: [Value] -> T.Text
inlineParenPrint l = "(" <++> T.intercalate " ," (map printValue l) <++> ")"

inlineBracketPrint :: [Value] -> T.Text
inlineBracketPrint l = "[" <++> T.intercalate " ," (map printValue l) <++> "]"
