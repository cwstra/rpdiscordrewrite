{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module HistoryDiceParser.NewResolveData
( tShow
, History(..)
, historySimplify
, historyTextShow
, Statistics(..)
, statToJSONText
, KeepDrop (..)
, toComparator
, vectorStyleShow
, NumTest(..)
, rangedIntegerToIntegral
, Dice(..)
, createDie
, addExplode
, addReroll
, addKeepDrop
, addSuccess
, DataStateT
, DataState
, historyBlank_
, historyNewEntry_
, historyUpdateRight_
, historyMap_
, historyZip_
, fromIntegralToStat
, infiniteTest
, explodingRoll
, rerollRoll
, keepDropResolve
, successResolve
, Assoc(..)
, UnaryFunction(..)
, InfixFunction(..)
, CalledFunction(..)
, Tree(..)
, treeRecShow
, getPrec
, Resolution(..)
, ResolveState
, roll
, simplify
, runResolve
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Loops
import           Control.Monad.State.Lazy
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import qualified Data.HashMap.Strict      as HM
import           Data.List
import           Data.Maybe
import qualified Data.Text.Lazy           as T
import           Data.Word
import           System.Random.PCG

import           Debug.Trace

import           General.UserNumber

-- ----------------------------------------------------------------------------
-- History data
-- ----------------------------------------------------------------------------

(<++>) = T.append

bracketed :: T.Text -> T.Text
bracketed mid = "{" <++> mid <++> "}"

parened :: T.Text -> T.Text
parened mid = "(" <++> mid <++> ")"

commaCal :: [T.Text] -> T.Text
commaCal = T.intercalate ", "

jsonKey :: T.Text -> T.Text
jsonKey  key = "\"" <++> key <++> "\":"

tShow :: (Show a) => a -> T.Text
tShow = T.pack . show

type History = [(T.Text, T.Text)]

historySimplify :: History -> History
historySimplify history = foldr historyStep [] history
  where
    historyStep :: (T.Text, T.Text) -> History -> History
    historyStep t [] = [t]
    historyStep t@(t1, t2) l@((l1,l2):ls)
        |l2 == t1 = (l1,t2):ls
        |otherwise = t:l

historyTextShow :: History -> T.Text
historyTextShow [] = T.empty
historyTextShow history = T.intercalate (T.singleton '\n') $ map tupMerge $ historySimplify history
  where
    tupMerge :: (T.Text, T.Text) -> T.Text
    tupMerge (before, after) = before <++> " = " <++> after

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

vectorStyleShow      :: Show a => [a] -> String
vectorStyleShow []   = "()"
vectorStyleShow [x]  = show x
vectorStyleShow list = "(" ++ intercalate ", " (map show list) ++ ")"

instance Show NumTest where
  show TestNone      = ""
  show (TestLeq n)   = "<="   ++ show (GReal n)
  show (TestLes n)   = "<"    ++ show (GReal n)
  show (TestGeq n)   = ">="   ++ show (GReal n)
  show (TestGre n)   = ">"    ++ show (GReal n)
  show (TestEq n)    = "=="   ++ vectorStyleShow n
  show (TestNeq n)   = "/="   ++ vectorStyleShow n
  show (TestIn m n)  = "In("  ++ show (GReal m) ++ "," ++ show (GReal n) ++ ")"
  show (TestOut m n) = "Out(" ++ show (GReal m) ++ "," ++ show (GReal n) ++ ")"

rangedIntegerToIntegral :: (Bounded a, Integral a) => Integer -> a
rangedIntegerToIntegral n
    |n > fromIntegral maxi = maxi
    |n < fromIntegral mini = mini
    |otherwise = fromInteger n
  where
    mini = minBound
    maxi = maxBound

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

type DataStateT = StateT (FrozenGen, [History], Statistics)
type DataState = DataStateT Identity

-- ----------------------------------------------------------------------------
-- Base Monad Access
-- ----------------------------------------------------------------------------

genM :: Monad m => (forall s. Gen s -> ST s a) -> DataStateT m a
genM f = do
    (gen, his, sta) <- get
    let (a, nGen) = withFrozen gen f
    put (nGen, his, sta)
    return a

historyBlank_ :: Monad m => DataStateT m ()
historyBlank_ = do
    (gen, cs, sta) <- get
    put (gen, []:cs, sta)

historyNewEntry_ :: Monad m => (T.Text, T.Text) -> DataStateT m ()
historyNewEntry_ nextChapter = do
    (gen, c:cs, sta) <- get
    put (gen, (nextChapter:c):cs, sta)

historyUpdateRight_ :: Monad m => T.Text -> DataStateT m ()
historyUpdateRight_ rightHand = do
    (gen, his, sta) <- get
    put (gen, updateAll his, sta)
  where
    updateAll :: [History] -> [History]
    updateAll [] = [updateRight []]
    updateAll (c:cs) = updateRight c : cs
    updateRight :: History -> History
    updateRight [] = [(rightHand, rightHand)]
    updateRight ((leftHand, _):c) = (leftHand, rightHand):c

historyMap_ :: Monad m => ((T.Text, T.Text) -> (T.Text, T.Text)) -> DataStateT m ()
historyMap_ f = do
    (gen, h:hs, sta) <- get
    put $ (gen, (map f h):hs, sta)

historyZip_ :: Monad m
            => Int
            -> ([History] -> History)
            -> DataStateT m ()
historyZip_ n f = do
    (gen, his, sta) <- get
    let (h, hs) = splitAt n his
    put (gen, f h:hs, sta)

historyPeel :: Monad m => DataStateT m History
historyPeel = do
    (gen, h:hs, sta) <- get
    put (gen, hs, sta)
    return h

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

statUpdate_ :: (Integral a, Integral b, Monad m) => (a, a) -> HM.HashMap b b -> DataStateT m ()
statUpdate_ k@(key1, key2) value = do
    (gen, his, sta) <- get
    put $ (gen, his, updateHelper sta)
  where
    updateHelper  :: HM.HashMap (Word, Word) (HM.HashMap Word Word)
                      -> HM.HashMap (Word, Word) (HM.HashMap Word Word)
    updateHelper  =  HM.insertWith mapAdd (fromIntegral key1, fromIntegral key2) (toWordMap value)

statUpdateMany_ :: (Integral a, Integral b, Monad m) => HM.HashMap (a, a) (HM.HashMap b b) -> DataStateT m ()
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

rollOne :: Monad m => Either Int [GeneralNumber] -> DataStateT m GeneralNumber
rollOne face = liftM f $ genM $ uniformR (mini, maxi)
  where
    rangeGen :: Either Int [GeneralNumber] -> (Int, Int, Int -> GeneralNumber)
    rangeGen (Left n) = (1, n, \x -> gReal x)
    rangeGen (Right l) = (0, length l -1, (l!!))
    (mini, maxi, f) = rangeGen face

rollPool   :: Monad m => Int -> Either Int [GeneralNumber] -> DataStateT m [GeneralNumber]
rollPool n = replicateM n . rollOne

trackedPool :: Monad m => Int -> Either Int [GeneralNumber] -> DataStateT m [GeneralNumber]
trackedPool n face@(Left m) = do
    res <- rollPool n face
    statUpdate_ (n, m) $ toHMap res
    return res
  where
    toWord :: GeneralNumber -> Word
    toWord (GReal (GSimp (GInt n))) = rangedIntegerToIntegral n
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
              -> DataStateT m (Either T.Text [GeneralNumber])

explodingRoll lastResults face test
    |infiniteTest face test = return $ Left "Infinitely exploding roll detected. Roll aborted"
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
    explodeStep :: Monad m => [[GeneralNumber]] -> DataStateT m [[GeneralNumber]]
    explodeStep (o@(l:_)) = do
        let c = successCount comp l
        next <- rollPool c face
        return $ next:o

rerollRoll :: Monad m
           => [GeneralNumber]
           -> Either Int [GeneralNumber]
           -> NumTest
           -> DataStateT m (Either T.Text [PossNumber])
rerollRoll lastResults face test
    |infiniteTest face test = return $ Left "Infinitely rerolling roll detected. Roll aborted"
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
    rerollStep :: Monad m => ([GeneralNumber], [[PossNumber]]) -> DataStateT m ([GeneralNumber], [[PossNumber]])
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

diceRoll :: Monad m => Dice -> DataStateT m (Either T.Text (T.Text, GeneralNumber))
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
    text list = parened $ commaCal $ map (tShow) list
    successed :: [PossNumber] -> (T.Text, GeneralNumber)
    successed list = (text list, successResolve list dieSucc)

-- ----------------------------------------------------------------------------
-- Function types
-- ----------------------------------------------------------------------------

data Assoc = AssocLeft | AssocRight | AssocNone

data UnaryFunction  = UnaryF {unaryParen :: Bool,
                              unaryRep :: T.Text,
                              unaryPrec :: Int,
                              unaryFun :: (Tree -> ResolveState Tree)}

instance Show UnaryFunction where
    show (UnaryF _ t _ _) = T.unpack t

data InfixFunction  = InfixF {infixParen :: Bool,
                              infixRep :: T.Text,
                              infixAssoc :: Assoc,
                              infixPrec :: Int,
                              infixFun :: (Tree -> Tree -> ResolveState Tree)}

instance Show InfixFunction where
    show (InfixF _ t _ _ _) = T.unpack t

data CalledFunction = CalledF {calledRep :: T.Text, calledFun :: [Tree] -> ResolveState Tree}

instance Show CalledFunction where
    show (CalledF t _) = T.unpack t

-- ----------------------------------------------------------------------------
-- Trees
-- ----------------------------------------------------------------------------

data Tree = TreeNum     GeneralNumber
          | TreeBool    Bool
          | TreeDie     Dice
          | TreePrefix  UnaryFunction Tree
          | TreeInfix   Tree InfixFunction Tree
          | TreePostfix Tree UnaryFunction
          | TreeFun     CalledFunction Tree
          | TreeVec     [Tree]

treeRecShow :: Int -> Tree -> String
treeRecShow n t = replicate n ' ' ++ treeStep t
  where
    next  = treeRecShow (n + 2)
    lined = intercalate "\n"
    mid p l = lined $ [p ++ show (length l)] ++ map next l
    vecDrop :: String -> String -> String -> [Tree] -> String
    vecDrop left right pre list = concat [left, mid pre list, right]
    treeStep :: Tree -> String
    treeStep (TreeNum n)         = show n
    treeStep (TreeBool b)        = show b
    treeStep (TreeDie d)         = show d
    treeStep (TreePrefix o t)    = show o ++ "\n" ++ next t
    treeStep (TreeInfix t1 o t2) = lined $ (show o):(map next [t1, t2])
    treeStep (TreePostfix t o)   = show o ++ "\n" ++ next t
    treeStep (TreeFun o t)       = show o ++ "\n" ++ next t
    treeStep (TreeVec l)         = vecDrop "(" ")" "vec" l

instance Show Tree where
    show = treeRecShow 0

getPrec :: Tree -> Maybe Int
getPrec (TreePrefix (UnaryF {unaryPrec = n}) _)   = Just n
getPrec (TreeInfix  _ (InfixF {infixPrec = n}) _) = Just n
getPrec (TreePostfix _ (UnaryF {unaryPrec = n}))  = Just n
getPrec _                                         = Nothing

-- ----------------------------------------------------------------------------
-- Resolution
-- ----------------------------------------------------------------------------

data Resolution a = Resolved a
                  | Failed T.Text

instance Show a => Show (Resolution a) where
    show (Resolved a) = show a
    show (Failed t)   = "Failed " ++ T.unpack t

instance Functor Resolution where
    fmap _ (Failed e) = Failed e
    fmap f (Resolved a) = Resolved $ f a

instance Applicative Resolution where
    pure a = Resolved a
    liftA2 f (Resolved a) (Resolved b) = Resolved $ f a b
    liftA2 f (Failed e) _ = Failed e
    liftA2 f _ (Failed e) = Failed e

instance Alternative Resolution where
    empty = Failed ""
    a@(Resolved _) <|> _ = a
    _ <|> a@(Resolved _) = a
    e@(Failed _) <|> _   = e

instance Monad Resolution where
    m >>= g = case m of
                Failed t -> Failed t
                Resolved a -> g a

instance MonadPlus Resolution

-- ----------------------------------------------------------------------------
-- ResolveState
-- ----------------------------------------------------------------------------

type ResolveState = DataStateT Resolution

simplify :: Tree -> ResolveState Tree
simplify input
    |TreePrefix (UnaryF {unaryFun = f}) child <- input = simplify child >>= f
    |TreeInfix child1 (InfixF {infixFun = f}) child2 <- input = do
        sChild1 <- simplify child1
        sChild2 <- simplify child2
        f child1 child2
    |TreePostfix child (UnaryF {unaryFun = f}) <- input = simplify child >>= f
    |TreeFun (CalledF text f) childVec <- input = do
        sChildVec <- simplify childVec
        let TreeVec children = sChildVec
        f children
    |otherwise = return input

wrappedRoll :: Dice -> ResolveState (T.Text, GeneralNumber)
wrappedRoll d = do
    res <- diceRoll d
    case res of
        Left t -> lift $ Failed t
        Right a -> return a

roll :: Tree -> ResolveState Tree
roll (TreeDie d) = do
    (trackedText, res) <- wrappedRoll d
    historyNewEntry_ (trackedText, tShow res)
    return $ TreeNum res
roll v@(TreeVec l) = if all noDice l then return v else do
    (t, resEl) <- liftM showVec $ mapM rollRec l
    historyNewEntry_ t
    return resEl
  where
    noDice :: Tree -> Bool
    noDice (TreeDie d) = False
    noDice (TreeVec l) = all noDice l
    noDice _           = True
    rollRec :: Tree -> ResolveState ((T.Text, T.Text), Tree)
    rollRec (TreeDie d) = liftM showNum $ wrappedRoll d
    rollRec (TreeVec l) = liftM showVec $ mapM rollRec l
    rollRec t           = return ((shown, shown), t)
      where
        shown = tShow t
    showNum :: (T.Text, GeneralNumber) -> ((T.Text, T.Text), Tree)
    showNum (t, n) = ((t, tShow n), TreeNum n)
    showVec :: [((T.Text, T.Text), Tree)] -> ((T.Text, T.Text), Tree)
    showVec l = finish $ foldr vecStep ("", "", []) l
      where
        vecStep :: ((T.Text, T.Text), Tree)
                -> (T.Text, T.Text, [Tree])
                -> (T.Text, T.Text, [Tree])
        vecStep ((t1, t2), tt) (b1, b2, bt) = (b1 <++> t1, b2 <++> t2, tt:bt)
        finish :: (T.Text, T.Text, [Tree]) -> ((T.Text, T.Text), Tree)
        finish (t1, t2, l) = ((t1, t2), TreeVec l)
roll t = return t

runResolve :: Word64
           -> Word64
           -> ResolveState Tree
           -> Resolution (Tree, (FrozenGen, [History], Statistics))
runResolve word1 word2 monad = runStateT initMonad (initFrozen word1 word2, [], HM.empty)
  where
    initMonad = do
        historyBlank_
        firstRes <- monad
        simple <- simplify firstRes
        historyUpdateRight_ $ tShow simple
        roll simple
