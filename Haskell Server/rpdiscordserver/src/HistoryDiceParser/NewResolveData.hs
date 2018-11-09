module HistoryDiceParser.NewResolveData
( History(..)
, historySimplify
, historyTextShow
, Statistics(..)
, statToJSONText
, KeepDrop (..)
, toComparator
, NumTest(..)
, Dice(..)
, createDie
, addExplode
, addReroll
, addKeepDrop
, addSuccess
, ResolveState
, historyUpdate_
, fromIntegralToStat
, infiniteTest
, explodingRoll
, rerollRoll
, keepDropResolve
, successResolve
, diceRoll
, Static
) where

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Loops
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Class
import qualified Data.HashMap.Strict      as HM
import           Data.List
import           Data.Maybe
import qualified Data.Text.Lazy           as T
import           Data.Word
import           System.Random.Mersenne.Pure64

import           Debug.Trace

import           General.UserNumber

-- ----------------------------------------------------------------------------
-- History data
-- ----------------------------------------------------------------------------

lbrac = T.singleton '{'
rbrac = T.singleton '}'
lparn = T.singleton '('
rparn = T.singleton ')'
comma = T.pack      ", "
eqsep = T.pack      " = "
quote = T.singleton '"'
colon = T.singleton ':'

(<++>) = T.append

bracketed :: T.Text -> T.Text
bracketed mid = lbrac <++> mid <++> rbrac

parened :: T.Text -> T.Text
parened mid = lparn <++> mid <++> rparn

commaCal :: [T.Text] -> T.Text
commaCal = T.intercalate comma

jsonKey :: T.Text -> T.Text
jsonKey  key = quote <++> key <++> quote <++> colon

tShow :: (Show a) => a -> T.Text
tShow = T.pack . show

type History = [(T.Text, T.Text)]

historySimplify :: History -> History
historySimplify history = foldr historyStep [] history
  where
    historyStep :: (T.Text, T.Text) -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
    historyStep t [] = [t]
    historyStep t@(t1, t2) l@((l1,l2):ls)
        |l2 == t1 = (l1,t2):ls
        |otherwise = t:l

historyTextShow :: History -> T.Text
historyTextShow [] = T.empty
historyTextShow history = T.intercalate (T.singleton '\n') $ map tupMerge $ historySimplify history
  where
    tupMerge :: (T.Text, T.Text) -> T.Text
    tupMerge (before, after) = before <++> eqsep <++> after

type Statistics = (HM.HashMap (Word, Word) (HM.HashMap Word Word))

statToJSONText :: Statistics -> T.Text
statToJSONText diceStat = bracketed $ commaCal $ map highMap $ HM.toList $ HM.map HM.toList diceStat
  where
    highMap :: ((Word, Word), [(Word, Word)])-> T.Text
    highMap ((pool, die), value) = (jsonKey $ tShow pool <++> T.singleton 'd' <++> tShow die) <++> mappedValue
      where
        mappedValue = bracketed $ commaCal $ map lowMap value
    lowMap :: (Word, Word) -> T.Text
    lowMap (result, count) = jsonKey (tShow result) <++> tShow count

-- ----------------------------------------------------------------------------
-- Dice data
-- ----------------------------------------------------------------------------

data KeepDrop = KeepHigh Int
              | DropHigh Int
              | KeepLow Int
              | DropLow Int
  deriving (Eq)

toListOp :: KeepDrop -> (a -> a -> Ordering) -> [a] -> [a]
toListOp kd sortFun l
    |KeepHigh n <- kd = drop (length l - n) $ sortBy sortFun l
    |DropHigh n <- kd = take (length l - n) $ sortBy sortFun l
    |KeepLow  n <- kd = take n              $ sortBy sortFun l
    |DropLow  n <- kd = drop n              $ sortBy sortFun l

instance Show KeepDrop where
  show (KeepHigh n) = "kh" ++ show n
  show (DropHigh n) = "dh" ++ show n
  show (KeepLow  n) = "kl" ++ show n
  show (DropLow  n) = "dl" ++ show n

data NumTest = TestNone
             | TestLeq GeneralRealNumber
             | TestLes GeneralRealNumber
             | TestGeq GeneralRealNumber
             | TestGre GeneralRealNumber
             | TestEq  [GeneralNumber]
             | TestNeq [GeneralNumber]
             | TestIn  GeneralRealNumber GeneralRealNumber
             | TestOut GeneralRealNumber GeneralRealNumber
  deriving (Eq)

inRange :: GeneralRealNumber -> GeneralRealNumber -> GeneralRealNumber -> Bool
inRange mini maxi x = mini <= x && x <= maxi

toComparator :: NumTest -> GeneralNumber -> Bool
toComparator test
    |TestNone    <- test = \_ -> False
    |TestLeq n   <- test = ordTest (<= n)
    |TestLes n   <- test = ordTest (< n)
    |TestGeq n   <- test = ordTest (>= n)
    |TestGre n   <- test = ordTest (> n)
    |TestEq  l   <- test = (`elem` l)
    |TestNeq l   <- test = not . (`elem` l)
    |TestIn  m n <- test = ordTest (inRange m n)
    |TestOut m n <- test = ordTest (not . inRange m n)
  where
    ordTest :: (GeneralRealNumber -> Bool) -> GeneralNumber -> Bool
    ordTest test x
      |GReal n <- x = test n
      |otherwise    = False

fancyListShow      :: [GeneralNumber] -> String
fancyListShow []   = "()"
fancyListShow [x]  = show x
fancyListShow list = "(" ++ intercalate ", " (map show list) ++ ")"

instance Show NumTest where
  show TestNone      = ""
  show (TestLeq n)   = "<="   ++ show (GReal n)
  show (TestLes n)   = "<"    ++ show (GReal n)
  show (TestGeq n)   = ">="   ++ show (GReal n)
  show (TestGre n)   = ">"    ++ show (GReal n)
  show (TestEq n)    = "=="   ++ fancyListShow n
  show (TestNeq n)   = "/="   ++ fancyListShow n
  show (TestIn m n)  = "In("  ++ show (GReal m) ++ "," ++ show (GReal n) ++ ")"
  show (TestOut m n) = "Out(" ++ show (GReal m) ++ "," ++ show (GReal n) ++ ")"

rangedIntegerToWord :: Integer -> Word
rangedIntegerToWord n
    |n > fromIntegral maxi = maxBound
    |n < fromIntegral mini = minBound
    |otherwise = fromInteger n
  where
    mini = minBound :: Word
    maxi = maxBound :: Word

data Dice = Die { poolSize::       Int
                , poolDis::        String
                , face::           Either Int [GeneralNumber]
                , faceDis::        String
                , exploding::      (NumTest, Bool) -- If True, display number, else hide it
                , reroll::         NumTest
                , keep_drop::      Maybe KeepDrop
                , success::        NumTest
                }

createDie :: Int -> String -> Either Int [GeneralNumber] -> String -> Dice
createDie poolnum poolstr facerep facestring =
          Die { poolSize       = poolnum
              , poolDis        = poolstr
              , face           = facerep
              , faceDis        = facestring
              , exploding      = (TestNone, False)
              , reroll         = TestNone
              , keep_drop      = Nothing
              , success        = TestNone
              }
addExplode                  :: Dice -> NumTest -> Bool -> Dice
addExplode die numTest bool = die {exploding = (numTest, bool)}

addReroll                   :: Dice -> NumTest -> Dice
addReroll  die numTest      = die {reroll = numTest}

addSuccess                  :: Dice -> NumTest -> Dice
addSuccess die numTest      = die {success = numTest}

addKeepDrop                 :: Dice -> KeepDrop -> Dice
addKeepDrop die kdt         = die {keep_drop=Just kdt}

showDieSuffix :: Dice -> String
showDieSuffix Die {exploding = expl, reroll = rerollTest, keep_drop=kd, success=succ} = suffix
  where
    parseGenTest :: String -> NumTest -> String
    parseGenTest pre test
      |TestEq _ <- test = pre ++ (drop (if pre == "" then 0 else 2) $ show test)
      |TestNone <- test = ""
      |otherwise = pre ++ show test
    parseKeepDrop :: Maybe KeepDrop -> String
    parseKeepDrop Nothing = ""
    parseKeepDrop (Just kd) = show kd
    parseExpl :: (NumTest, Bool) -> String
    parseExpl (explT, explB)
      |explB = parseGenTest "!" explT
      |TestNone <- explT = ""
      |otherwise = "!"
    suffix = parseExpl expl ++ parseGenTest "r" rerollTest ++ parseKeepDrop kd ++ parseGenTest "" succ

instance Show Dice where
  show d@Die {poolDis=pool, faceDis=fac} = pool ++ "d" ++ fac ++ showDieSuffix d

-- ----------------------------------------------------------------------------
-- Monad Representation
-- ----------------------------------------------------------------------------

type ResolveStateT  = StateT (PureMT, History, Statistics)
type ResolveState   = ResolveStateT Identity

-- ----------------------------------------------------------------------------
-- Base Monad Access
-- ----------------------------------------------------------------------------

genM :: Monad m => (PureMT -> (a, PureMT)) -> ResolveStateT m a
genM f = do
    (gen, his, sta) <- get
    let (a, nGen) = f gen
    put (nGen, his, sta)
    return a

--Remember: This generates an integer in [mini, maxi)
intRange :: Monad m => Int -> Int -> ResolveStateT m Int
intRange mini maxi = do
    res <- iterateWhile (>= maxRange) (genM randomWord64)
    return $ fromIntegral res `mod` range + mini
  where
    range = maxi - mini
    maxWord = maxBound :: Word64
    maxRange = maxWord - maxWord `mod` fromIntegral range

historyUpdate_ :: Monad m => (T.Text, T.Text) -> ResolveStateT m ()
historyUpdate_ nextChapter = do
    (gen, his, sta) <- get
    put $ (gen, nextChapter:his, sta)

toWordMap :: (Integral a) => HM.HashMap a a -> HM.HashMap Word Word
toWordMap hashMap = HM.foldlWithKey' wordStep HM.empty hashMap
  where
    wordStep :: (Integral a) => HM.HashMap Word Word -> a -> a -> HM.HashMap Word Word
    wordStep wordMap k v = HM.insert (fromIntegral k) (fromIntegral v) wordMap

toTupWordMap :: (Integral a) => HM.HashMap (a, a) b -> HM.HashMap (Word, Word) b
toTupWordMap hashMap = HM.foldlWithKey' wordStep HM.empty hashMap
  where
    wordStep :: (Integral a) => HM.HashMap (Word, Word) b -> (a, a) -> b -> HM.HashMap (Word, Word) b
    wordStep wordMap (key1, key2) value = HM.insert (fromIntegral key1, fromIntegral key2) value wordMap

fromIntegralToStat m = toTupWordMap $ HM.map toWordMap m

mapAdd :: HM.HashMap Word Word -> HM.HashMap Word Word -> HM.HashMap Word Word
mapAdd map1 map2 = HM.unionWith (+) map1 map2

statUpdate_ :: (Integral a, Integral b, Monad m) => (a, a) -> HM.HashMap b b -> ResolveStateT m ()
statUpdate_ k@(key1, key2) value = do
    (gen, his, sta) <- get
    put $ (gen, his, updateHelper sta)
  where
    updateHelper  :: HM.HashMap (Word, Word) (HM.HashMap Word Word)
                      -> HM.HashMap (Word, Word) (HM.HashMap Word Word)
    updateHelper  =  HM.insertWith mapAdd (fromIntegral key1, fromIntegral key2) (toWordMap value)

statUpdateMany_ :: (Integral a, Integral b, Monad m) => HM.HashMap (a, a) (HM.HashMap b b) -> ResolveStateT m ()
statUpdateMany_ intMap = do
    (gen, his, sta) <- get
    put $ (gen, his, updateHelper sta)
  where
    updateHelper  :: HM.HashMap (Word, Word) (HM.HashMap Word Word)
                      -> HM.HashMap (Word, Word) (HM.HashMap Word Word)
    updateHelper  =  HM.unionWith mapAdd (fromIntegralToStat intMap)

-- ----------------------------------------------------------------------------
-- Rolling Functions
-- ----------------------------------------------------------------------------

rollOne :: Monad m => Either Int [GeneralNumber] -> ResolveStateT m GeneralNumber
rollOne face = liftM f $ intRange mini maxi
    {-Non-liftM
    res <- intRange mini maxi
    return $ f res-}
  where
    rangeGen :: Either Int [GeneralNumber] -> (Int, Int, Int -> GeneralNumber)
    --Note: right bound of intRange is non-inclusive, so inflate right
    --element
    rangeGen (Left n) = (1, n+1, \x -> gReal x)
    rangeGen (Right l) = (0, length l, (l!!))
    (mini, maxi, f) = rangeGen face

rollPool   :: Monad m => Int -> Either Int [GeneralNumber] -> ResolveStateT m [GeneralNumber]
rollPool n = replicateM n . rollOne

trackedPool :: Monad m => Int -> Either Int [GeneralNumber] -> ResolveStateT m [GeneralNumber]
trackedPool n face@(Left m) = do
    res <- rollPool n face
    statUpdate_ (n, m) $ toHMap res
    return res
  where
    toWord :: GeneralNumber -> Word
    toWord (GReal (GSimp (GInt n))) = rangedIntegerToWord n
    toHMap :: [GeneralNumber] -> HM.HashMap Word Word
    toHMap l = HM.singleton (toWord $ sum l) 1
trackedPool n face = rollPool n face

-- infiniteTest face test == True -> roll will infinitely explode or reroll
infiniteTest :: Either Int [GeneralNumber] -> NumTest -> Bool
infiniteTest (Left n) test
    |TestEq _          <- test   = listTest
    |TestNeq _         <- test   = listTest
    |TestOut mini maxi <- test   = maxi < 1 || genN < maxi || mini > (GSimp $ GInt $ floor maxi)
    |otherwise                  = comp 1 && comp (GReal genN)
  where
    genN = GSimp $ GInt $ fromIntegral n
    comp = toComparator test
    listTest = all comp $ map gReal [1..n]
infiniteTest (Right l) test = all comp l
  where
    comp = toComparator test

reduceMultiList :: [[a]] -> Either b [a]
reduceMultiList l = Right $ foldl' (\b a -> a ++ b) [] l

explodingRoll :: Monad m
              => [GeneralNumber]
              -> Either Int [GeneralNumber]
              -> NumTest
              -> ResolveStateT m (Either ResolveException [GeneralNumber])

explodingRoll lastResults face test
    |infiniteTest face test = return $ Left $ resolveException "Infinitely exploding roll detected. Roll aborted"
    |otherwise = do
        listO'Lists <- iterateUntilM stopMultiList explodeStep [lastResults]
        return $ reduceMultiList listO'Lists
  where
    successCount :: (a -> Bool) -> [a] -> Int
    successCount comp list = length $ [i | i <- list, comp i]
    stopMultiList :: [[a]] -> Bool
    stopMultiList ([]:_) = True
    stopMultiList a    = False
    comp = toComparator test
    explodeStep :: Monad m => [[GeneralNumber]] -> ResolveStateT m [[GeneralNumber]]
    explodeStep (o@(l:_)) = do
        let c = successCount comp l
        next <- rollPool c face
        return $ next:o

rerollRoll :: Monad m
           => [GeneralNumber]
           -> Either Int [GeneralNumber]
           -> NumTest
           -> ResolveStateT m (Either ResolveException [PossNumber])
rerollRoll lastResults face test
    |infiniteTest face test = return $ Left $ resolveException "Infinitely rerolling roll detected. Roll aborted"
    |otherwise = do
        (_, listO'Lists) <- iterateUntilM rerollPred rerollStep (lastResults, [])
        return $ reduceMultiList listO'Lists
  where
    comp = toComparator test
    rerollPred :: ([GeneralNumber], [[PossNumber]]) -> Bool
    rerollPred ([], _) = True
    rerollPred x       = False
    rerollify :: [GeneralNumber] -> (Int, [PossNumber])
    rerollify = foldr possStep (0, [])
      where
        possStep :: GeneralNumber -> (Int, [PossNumber]) -> (Int, [PossNumber])
        possStep n (c, l)
            |comp n    = (c+1, Dropped n:l)
            |otherwise = (c,   Kept n:l)
    rerollStep :: Monad m => ([GeneralNumber], [[PossNumber]]) -> ResolveStateT m ([GeneralNumber], [[PossNumber]])
    rerollStep (l, ls) = do
        let (c, ln) = rerollify l
        next <- rollPool c face
        return $ (next, ln:ls)

keepDropResolve :: [PossNumber] ->  Maybe KeepDrop -> [PossNumber]
keepDropResolve lastResults Nothing = lastResults
keepDropResolve lastResults (Just kd)  = endRes
  where
    initPrec :: (Int, PossNumber) -> Maybe (Int, GeneralRealNumber)
    initPrec (ind, Kept (GReal n)) = Just (ind, n)
    initPrec _                     = Nothing
    indexedIn  = zip [0..] lastResults
    realList   = mapMaybe initPrec indexedIn
    keptInds   = map fst $ toListOp kd (\(_, a) (_, b) -> compare a b) realList
    checkDrop :: [Int] -> (Int, PossNumber) -> PossNumber
    checkDrop inds (ind, Kept n)
        |ind `elem` inds = Kept n
        |otherwise       = Dropped n
    checkDrop _ (_, x) = x
    endRes = map (checkDrop keptInds) indexedIn

successResolve :: [PossNumber] -> NumTest -> GeneralNumber
successResolve lastResult TestNone = sum $ map removePoss lastResult
  where
    removePoss :: PossNumber -> GeneralNumber
    removePoss (Kept n) = n
    removePoss _        = 0
successResolve lastResult test = gReal $ length filteredList
  where
    succPrec :: PossNumber -> Bool
    succPrec (Kept n) = toComparator test n
    succPrec _        = False
    filteredList = filter succPrec lastResult

diceRoll :: Monad m => Dice -> ResolveStateT m (Either ResolveException (T.Text, GeneralNumber))
diceRoll Die { poolSize = diePool
             , face = dieFace
             , exploding = (dieExpl, _)
             , reroll = dieReroll
             , keep_drop = dieKD
             , success = dieSucc}
  = do
    initialRoll <- trackedPool diePool dieFace
    explRoll <- explodingRoll initialRoll dieFace dieExpl
    case explRoll of
      Left e -> return $ Left e
      Right explList -> do
        rerolled <- rerollRoll explList dieFace dieReroll
        case rerolled of
          Left e -> return $ Left e
          Right rerollL -> return $ Right $ successed $ keepDropResolve rerollL dieKD
  where
    text :: [PossNumber] -> T.Text
    text list = parened $ commaCal $ map (T.pack . show) list
    successed :: [PossNumber] -> (T.Text, GeneralNumber)
    successed list = (text list, successResolve list dieSucc)

-- ----------------------------------------------------------------------------
-- Static Data
-- ----------------------------------------------------------------------------

data Static = StaticNum  GeneralNumber
            | StaticBool Bool
            | StaticDice Dice
            | StaticVect [Static]

instance Show OpVector where
    show (StaticNum n)  = show n
    show (StaticBool b) = show b
    show (StaticDice d) = show d
    show (StaticVect v) = "(" ++ intercalate ", " (map show v) ++ ")"
