{-# LANGUAGE OverloadedStrings #-}
module NewDiceParser.Monads
( RollerStateT
, RollRes
, putVariable
, getVariable
, subSpace
, emptyRollerState
, rollDie
) where

import           Control.Monad
import           Control.Monad.Identity
import           Data.Word
import           Data.List

import           Control.Monad.State.Lazy
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Text.Lazy as T
import           System.Random.Mersenne.Pure64

import           General.UserNumber
import           NewDiceParser.Data
import           NewDiceParser.Functions

emptyRollerState :: Monad m => Word64 -> RollerStateT m ()
emptyRollerState seed = put (HM.empty, pureMT seed)

putVariable :: Monad m => T.Text -> Value -> RollerStateT m ()
putVariable t v = do
  (ns, mt) <- get
  put (HM.insert t v ns, mt)

getVariable :: Monad m => T.Text -> RollerStateT m (Maybe Value)
getVariable t = do
  (ns, _) <- get
  return $ HM.lookup t ns

subSpace :: Monad m => RollerStateT m () -> RollerStateT m a -> RollerStateT m a
subSpace lets action = do
  orig <- get
  lets
  out <- action
  put orig
  return out

wrapPure :: Monad m => (PureMT -> (a, PureMT)) -> RollerStateT m a
wrapPure f = do
  (ns, mt) <- get
  let (a, nmt) = f mt
  put (ns, nmt)
  return a

randomUnder :: Monad m => Word64 -> RollerStateT m Word64
randomUnder m = do
  original <- wrapPure randomWord64
  if original <= maxBound - (maxBound `mod` m)
     then return $ original `mod` m
     else randomUnder m

oneDie :: Face -> RollRes GeneralNumber
oneDie (Left i) = do
  w <- randomUnder $ fromIntegral i - 1
  return $ fromIntegral $ w + 1
oneDie (Right l) = do
  w <- randomUnder $ fromIntegral $ length l
  return $ l !! (fromIntegral w)

multiDie :: Int -> Face -> RollRes [GeneralNumber]
multiDie p f = replicateM p $ oneDie f

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

exploDie :: Face -> DiceTest -> [GeneralNumber] -> RollRes [GeneralNumber]
exploDie f dt i = do
  let c = sum $ map (boolToInt . diceTest dt) i
  if c > 0
     then do
       nextPool <- multiDie c f
       r <- exploDie f dt nextPool
       return $ i ++ r
     else return i

rerollDie :: Face -> DiceTest -> [GeneralNumber] -> RollRes [PossNumber]
rerollDie f dt i = do
  let (count, posses) = countNWrap i
  if count > 0
     then do
       nextPool <- multiDie count f
       res <- rerollDie f dt nextPool
       return $ posses ++ res
     else return posses
  where
    test = diceTest dt
    countNWrap :: [GeneralNumber] -> (Int, [PossNumber])
    countNWrap ns = foldl' cNWStep (0, []) ns
    cNWStep :: (Int, [PossNumber]) -> GeneralNumber -> (Int, [PossNumber])
    cNWStep (c, ps) n
      |test n    = (c + 1, Dropped n : ps)
      |otherwise = (c,     Kept n    : ps)

kdDie :: KeepDrop -> [PossNumber] -> [PossNumber]
kdDie kd l
  |KDNone <- kd = l
  |otherwise = map removeDropped indexedList
  where
    sortFun :: Ord a => KeepDrop -> a -> a -> Ordering
    sortFun (KeepHigh _) m n = compare n m
    sortFun (KeepLow  _) m n = compare m n
    sortFun (DropHigh _) m n = compare n m
    sortFun (DropLow  _) m n = compare m n
    keptReal :: PossNumber -> Maybe GeneralRealNumber
    keptReal (Kept (GReal n)) = Just n
    keptReal _                = Nothing
    genSortFun kd (_, m) (_, n)
      |Just r <- keptReal m, Just s <- keptReal n = sortFun kd r s
      |Just _ <- keptReal m                      = LT
      |Just _ <- keptReal n                      = GT
      |otherwise                                = EQ
    kdFun :: KeepDrop -> [a] -> [a]
    kdFun (KeepHigh n) = take n
    kdFun (KeepLow n)  = take n
    kdFun (DropHigh n) = drop n
    kdFun (DropLow n)  = drop n
    indexedList = zip [0..] l
    indexSet = HS.fromList $ kdFun kd $ map fst $ sortBy (genSortFun kd) indexedList
    removeDropped :: (Int, PossNumber) -> PossNumber
    removeDropped (i, n)
      |Kept d <- n, not (i `HS.member` indexSet) = Dropped d
      |otherwise                                = n

endResult :: DiceTest -> [PossNumber] -> ([PossNumber], GeneralNumber)
endResult dt ps = (lastPS, total lastPS)
  where
    possStep :: (GeneralNumber -> Bool) -> PossNumber -> PossNumber
    possStep dt p
      |Kept n <- p, not $ dt n = Dropped n
      |otherwise                = p
    possWrap :: DiceTest -> [PossNumber] -> [PossNumber]
    possWrap DNone = id
    possWrap dt    = map $ possStep $ diceTest dt
    lastPS = possWrap dt ps
    sumAmount :: DiceTest -> GeneralNumber -> GeneralNumber
    sumAmount DNone n = n
    sumAmount _     _ = 1
    sA = sumAmount dt
    onlyKept :: GeneralNumber -> PossNumber -> GeneralNumber
    onlyKept s (Kept n) = s + sA n
    onlyKept s _        = s
    total :: [PossNumber] -> GeneralNumber
    total p = foldl' onlyKept 0 p

rollDie :: Dice -> RollRes (Historical GeneralNumber)
rollDie d = do
  firstRoll <- multiDie (pool d) (face d)
  exploRoll <- exploDie (face d) (exTest d) firstRoll
  rerollRoll <- rerollDie (face d) (rrTest d) exploRoll
  let kdRoll = kdDie (kdTest d) rerollRoll
  let (endRoll, endSum) = endResult (scTest d) kdRoll
  return $ (endSum, [("(" <++> T.intercalate "," (map tShow endRoll) <++> ")", tShow endSum)])
