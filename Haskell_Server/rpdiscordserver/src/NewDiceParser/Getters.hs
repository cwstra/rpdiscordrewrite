{-# LANGUAGE OverloadedStrings #-}
module NewDiceParser.Getters
( getNumber
, getReal
, getInteger
, getInt
, getIntegralValue
, getNumericVec
, getNumeric
, getRealVec
, getFace
) where

import Control.Applicative
import Control.Monad
import Data.List

import General.UserNumber
import NewDiceParser.Data

allOrNothing :: (a -> Maybe b) -> [a] -> Maybe [b]
allOrNothing f = mapM (aONStep f)
  where
    aONStep :: (a -> Maybe b) -> a -> Maybe b
    aONStep f a
      |Just b <- f a = Just b
      |otherwise    = Nothing

getNumber :: Value -> Maybe GeneralNumber
getNumber (Number n) = Just n
getNumber _          = Nothing

getReal :: Value -> Maybe GeneralRealNumber
getReal (Number (GReal n)) = Just n
getReal _                  = Nothing

getInteger :: Value -> Maybe Integer
getInteger (Number (GReal (GSimp (GInt n)))) = Just n
getInteger _                                 = Nothing

getInt :: Value -> Maybe Int
getInt v = getInteger v >>= limitFace

getIntegralValue :: Value -> Maybe IntegralValue
getIntegralValue (Vector d v)                      = liftM (IVector d) $ allOrNothing getIntegralValue v
getIntegralValue (Number (GReal (GSimp (GInt n)))) = Just $ IScalar n
getIntegralValue _                                 = Nothing

limitFace :: Integer -> Maybe Int
limitFace n
  |n > bigInt = Just maxBound
  |n > 0      = Just $ fromIntegral n
  |otherwise  = Nothing
  where
    bigInt :: Integer
    bigInt = fromIntegral (maxBound :: Int)

getNumericVec :: Value -> Maybe [GeneralNumber]
getNumericVec (Vector _ v)    = allOrNothing getNumber v
getNumericVec _               = Nothing

getNumeric :: Value -> Maybe Numeric
getNumeric (Vector d v) = liftM (NVector d) $ allOrNothing getNumeric v
getNumeric (Number n)   = Just $ Scalar n
getNumeric _            = Nothing

getRealVec :: Value -> Maybe [GeneralRealNumber]
getRealVec (Vector _ v)    = allOrNothing getReal v
getRealVec _               = Nothing

getFace :: Value -> Maybe Face
getFace v = liftM Left (getInt v) <|> liftM Right (getNumericVec v)
