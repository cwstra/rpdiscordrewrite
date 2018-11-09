module HistoryDiceParser.NewResolveDataSpec where

import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State.Lazy
import           Data.Aeson
import qualified Data.ByteString                  as B
import           Data.Char
import qualified Data.HashMap.Strict              as HM
import           Data.List
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy                   as T
import           System.Random.Mersenne.Pure64
import           Test.Hspec
import           Test.QuickCheck

import           Debug.Trace

import           General.UserNumber
import           HistoryDiceParser.NewResolveData

{-
historyTextShow
statToJSONText
createDie
addExplode
addReroll
addKeepDrop
addSuccess
historyUpdate_
diceRoll
-}

-------------------------------------------------------------------------------
--Generators
-------------------------------------------------------------------------------

listOf2 :: Gen a -> Gen [a]
listOf2 gen = sized $ \n -> do
    k <- choose (2,2 `max` n)
    vectorOf k gen


arbitraryHistory :: Gen History
arbitraryHistory = sized $ \n -> do
    (_, h) <- iterateUntilM histPred histStep (n, [])
    return h
  where
    histPred :: (Int, History) -> Bool
    histPred (0, _) = True
    histPred _      = False
    histStep :: (Int, History) -> Gen (Int, History)
    histStep (sizeI, []) = liftM (\x -> (sizeI, [x])) textPairGen
    histStep (sizeI, h@((h1, _):_)) = do
      disc <- arbitrary :: Gen Bool
      case disc of
        True -> do
          newText <- textPairGen
          return (sizeI - 1, newText:h)
        False -> do
          newText <- textGen
          return (sizeI - 1, (newText,h1):h)

arbitraryStats :: Gen Statistics
arbitraryStats = sized $ \s -> do
    n <- choose (1, s)
    k <- replicateM n $ keys $ s `div` 2 + 1
    e <- mapM (entry $ s `div` 4 + 1) k
    return $ fromIntegralToStat $ HM.fromList e
  where
    keys :: Int -> Gen (Int, Int)
    keys m = do
      n <- choose (1, m)
      f <- choose (1,m)
      return (n, f)
    entry :: Int -> (Int, Int) -> Gen ((Int, Int), HM.HashMap Int Int)
    entry s t@(n, f) = do
      i <- choose (1, s)
      v <- replicateM i $ choose (n, f * n)
      r <- replicateM i $ choose (1, s)
      return (t, HM.fromList $ zip v r)

arbitraryNumTest :: Maybe (Int, Int) -> Gen NumTest
arbitraryNumTest Nothing = return TestNone
arbitraryNumTest (Just x) = oneof $ (map (singleGen x) [TestLeq, TestLes, TestGeq, TestGre]) ++
                                    (map (listGen x) [TestEq, TestNeq]) ++
                                    (map (doubleGen x) [TestIn, TestOut])
  where
    singleGen :: (Int, Int) -> (GeneralRealNumber -> NumTest) -> Gen NumTest
    singleGen (m, n) f = do
        selected <- choose (m, n)
        return $ f $ GSimp $ GInt $ fromIntegral selected
    doubleGen :: (Int, Int) -> (GeneralRealNumber -> GeneralRealNumber -> NumTest) -> Gen NumTest
    doubleGen (m, n) f = do
        l <- replicateM 2 $ choose (m, n)
        let [smaller, bigger] = l
        return $ f (GSimp $ GInt $ fromIntegral smaller) (GSimp $ GInt $ fromIntegral bigger)
    listGen :: (Int, Int) -> ([GeneralNumber] -> NumTest) -> Gen NumTest
    listGen (m, n) f = do
        subList <- sublistOf [m..n]
        return $ f $ map (GReal . GSimp . GInt . fromIntegral) $ subList

arbitraryListTest :: Maybe [GeneralNumber] -> Gen NumTest
arbitraryListTest Nothing = return TestNone
arbitraryListTest (Just l) = oneof $ (map (listGen l) [TestEq, TestNeq])
  where
    listGen :: [GeneralNumber] -> ([GeneralNumber] -> NumTest) -> Gen NumTest
    listGen l f = do
        subList <- sublistOf l
        return $ f subList

arbitraryKD :: Maybe Int -> Gen (Maybe KeepDrop)
arbitraryKD Nothing = return Nothing
arbitraryKD (Just maxi) = do
    n <- choose(1, maxi)
    kd <- elements [KeepHigh, KeepLow, DropHigh, DropLow]
    return $ Just $ kd n

diceWithProp :: (Int, Either Int [GeneralNumber])
             -> Gen NumTest
             -> Gen NumTest
             -> Gen (Maybe KeepDrop)
             -> Gen NumTest
             -> Gen Dice
diceWithProp (n, f) explGen rerollGen kdGen succGen = do
    expl <- explGen
    rero <- rerollGen
    kd   <- kdGen
    succ <- succGen
    return $ Die { poolSize  = n
                 , poolDis   = show n
                 , face      = f
                 , faceDis   = faceDisplay f
                 , exploding = (expl, False)
                 , reroll    = rero
                 , keep_drop = kd
                 , success   = succ}
  where
    faceDisplay :: Either Int [GeneralNumber] -> String
    faceDisplay (Left n) = show n
    faceDisplay (Right l) = show l

arbitraryFaceDie :: Bool -> Bool -> Bool -> Bool -> Gen Dice
arbitraryFaceDie explB rerollB kdB succB = sized $ \s -> do
    n <- choose (1, s)
    f <- choose (2, s)
    diceWithProp (n, Left f) (bToNT explB n f) (bToNT rerollB n f) (bToKD kdB n) (bToNT succB n f)
  where
    bToNT :: Bool -> Int -> Int -> Gen NumTest
    bToNT b n f= arbitraryNumTest $ if b then Just (1, f) else Nothing
    bToKD :: Bool -> Int -> Gen (Maybe KeepDrop)
    bToKD b n = arbitraryKD $ if b then Just n else Nothing

arbitraryIntNumber :: Gen GeneralSimpleNumber
arbitraryIntNumber = liftM GInt arbitrary

arbitraryNonZero  :: Gen GeneralSimpleNumber
arbitraryNonZero   = iterateWhile (== 0) arbitraryIntNumber

arbitraryFloNumber :: Gen GeneralSimpleNumber
arbitraryFloNumber = liftM GFlo arbitrary

arbitrarySimpNumber :: Gen GeneralRealNumber
arbitrarySimpNumber = liftM GSimp $ frequency [(2, arbitraryIntNumber), (1, arbitraryFloNumber)]

arbitraryRatNumber :: Gen GeneralRealNumber
arbitraryRatNumber = liftM (reduce . fromRational) arbitrarySizedFractional

arbitraryRealNumber :: Gen GeneralRealNumber
arbitraryRealNumber = frequency [(2, arbitrarySimpNumber), (1, arbitraryRatNumber)]

arbitraryComplexNumber :: Gen GeneralNumber
arbitraryComplexNumber = liftM (\[x, y] -> GComp $ GC x y) $ replicateM 2 arbitraryRealNumber
arbitraryGeneralNumber :: Gen GeneralNumber
arbitraryGeneralNumber = frequency [(7, liftM GReal arbitraryRealNumber), (1, arbitraryComplexNumber)]

arbitraryNonFloat :: Gen GeneralNumber
arbitraryNonFloat = frequency [(7, liftM GReal arbR), (1, arbC)]
  where
    arbR :: Gen GeneralRealNumber
    arbR = frequency [(2, liftM GSimp arbitraryIntNumber), (1, arbitraryRatNumber)]
    arbC :: Gen GeneralNumber
    arbC = liftM (\[x, y] -> GReal x + imagUnit * GReal y) (replicateM 2 arbR)

arbitraryListDie :: Bool -> Bool -> Bool -> Bool -> Gen Dice
arbitraryListDie explB rerollB kdB succB = sized $ \s -> do
    n <- choose (1, s)
    lSize <- choose (1, s)
    l    <- replicateM lSize arbitraryGeneralNumber
    diceWithProp (n, Right l) (bToNT explB l) (bToNT rerollB l) (bToKD kdB n) (bToNT succB l)
  where
    bToNT :: Bool -> [GeneralNumber] -> Gen NumTest
    bToNT b l = arbitraryListTest $ if b then Just l else Nothing
    bToKD :: Bool -> Int -> Gen (Maybe KeepDrop)
    bToKD b n = arbitraryKD $ if b then Just n else Nothing

arbitraryDie :: Bool -> Bool -> Bool -> Bool -> Gen Dice
arbitraryDie e r k s = oneof [arbitraryFaceDie e r k s, arbitraryListDie e r k s]

faceGen :: Either Int [GeneralNumber] -> Gen GeneralNumber
faceGen (Left f) = choose (1, f) >>= \x -> return $ gReal x
faceGen (Right l) = elements l

arbitraryIndefiniteRoll :: Bool -> Gen ([GeneralNumber], Either Int [GeneralNumber], NumTest)
arbitraryIndefiniteRoll exploding = sized $ \s -> do
    n <- choose (1, s)
    face <- oneof [choose (1, s) >>= \x -> return $ Left $ x,
                  listOf1 arbitraryGeneralNumber >>= \x -> return $ Right x]
    l <- replicateM n $ faceGen face
    t <- testGen exploding face
    return (l, face, t)
  where
    testGen :: Bool -> Either Int [GeneralNumber] -> Gen NumTest
    testGen False _         = return TestNone
    testGen _     (Left n)  = arbitraryNumTest $ Just (1, n)
    testGen _     (Right l) = arbitraryListTest $ Just l

droppedOrKept :: GeneralNumber -> Gen PossNumber
droppedOrKept n = elements [Kept, Dropped] >>= \f -> return $ f n

arbitraryKDTest :: Bool -> Gen ([PossNumber], Maybe KeepDrop)
arbitraryKDTest kd = do
    generals <- listOf1 arbitraryGeneralNumber
    posses <- mapM droppedOrKept generals
    kd <- arbitraryKD $ if kd then Just (length posses) else Nothing
    return (posses, kd)

arbitrarySuccess :: Bool -> Gen ([PossNumber], NumTest)
arbitrarySuccess succ = sized $ \s -> do
    generals <- listOf1 $ if succ then arbitraryGeneralNumber else arbitraryNonFloat
    posses <- mapM droppedOrKept generals
    [m, n] <- liftM sort $ replicateM 2 $ choose (1, s)
    test <- arbitraryNumTest $ if succ then Just (m, n) else Nothing
    return (posses, test)


-------------------------------------------------------------------------------
--Pure Testing functions
-------------------------------------------------------------------------------


eqMerge :: T.Text -> T.Text -> T.Text
eqMerge t1 t2 = T.concat [t1, T.pack " = ", t2]

textGen  :: Gen T.Text
textGen = liftM T.pack $ listOf $ choose ('0', '~')

textPairGen :: Gen (T.Text, T.Text)
textPairGen = do
    [l1, l2] <- replicateM 2 textGen
    return (l1, l2)

nonRepeated :: History -> Bool
nonRepeated = snd . foldr repStep (Nothing, True)
  where
    repStep :: (T.Text, T.Text) -> (Maybe (T.Text, T.Text), Bool) -> (Maybe (T.Text, T.Text), Bool)
    repStep h (Nothing, True) = (Just h, True)
    repStep h@(h1, _) (Just (_, l2),  True)
        |h1 == l2  = (Nothing, False)
        |otherwise = (Just h, True)
    repStep _ (_, False) = (Nothing, False)

splitD :: [String] -> Char -> [String]
splitD (s:ss) c
    |c == 'd'  = "":s:ss
    |otherwise = (s++[c]):ss

verifyStats :: Statistics -> Bool
verifyStats text
  |Nothing <- eval = False
  |Just r <- eval = checkOuterKeys r && checkInnerKeys r
  where
    byte = encodeUtf8 $ statToJSONText text
    eval = decode byte :: Maybe (HM.HashMap String (HM.HashMap String Integer))
    checkOuterKeys :: HM.HashMap String a -> Bool
    checkOuterKeys m = all (all (all isDigit)) $ map (foldl' splitD [""]) $ HM.keys m
    checkInnerKeys :: HM.HashMap a (HM.HashMap String b) -> Bool
    checkInnerKeys m = all (all (all isDigit)) $ map HM.keys $ HM.elems m

addHistories :: (History, History) -> History
addHistories (h1, h2)
  |(_, h, _) <- execState (mapM_ historyUpdate_ h2) (pureMT 1, h1, HM.empty) = h

countPred :: (a -> Bool) -> [a] -> Int
countPred test = foldl' (\i e  -> i + if test e then 1 else 0) 0

countKept :: [PossNumber] -> Int
countKept = foldl' countKept 0
  where
    countKept :: Int -> PossNumber -> Int
    countKept s (Kept _) = s + 1
    countKept s _ = s

checkDropped :: NumTest -> [PossNumber] -> Bool
checkDropped pred = all toPossPred
  where
    comp = toComparator pred
    toPossPred :: PossNumber -> Bool
    toPossPred (Dropped n) = comp n
    toPossPred _           = True

countRemoved :: Maybe KeepDrop -> [PossNumber] -> Int
countRemoved Nothing _ = 0
countRemoved (Just kd) l
    |KeepHigh n <- kd = c + r - min r n
    |KeepLow  n <- kd = c + r - min r n
    |DropHigh n <- kd = c + min r n
    |DropLow  n <- kd = c + min r n
  where
    (r, c) = foldl' realCount (0,0) l
    realCount :: (Int, Int) -> PossNumber -> (Int, Int)
    realCount (r, c) (Kept (GReal _)) = (r + 1, c)
    realCount (r, c) (Kept _)         = (r, c + 1)
    realCount t      _                = t

filterDropped :: [PossNumber] -> [GeneralNumber]
filterDropped = map removePoss
  where
    removePoss :: PossNumber -> GeneralNumber
    removePoss (Kept n) = n
    removePoss _        = 0

filterTest :: NumTest -> [PossNumber] -> GeneralNumber
filterTest test = gReal . length . (filter possPred)
  where
    comp = toComparator test
    possPred :: PossNumber -> Bool
    possPred (Kept n) = comp n
    possPred _        = False

-------------------------------------------------------------------------------
--Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "HistoryDiceParser.NewResolveData" $ do

    describe "historySimplify" $ do

      it "merges identical after-before pairs in sequential elements" $ forAll arbitraryHistory $
        nonRepeated . historySimplify

    describe "historyTextShow" $ do

      it "returns empty text for empty histories" $ do
        historyTextShow [] `shouldBe` T.empty

      it "properly displays non-empty histories" $ forAll arbitraryHistory $
        \l -> historyTextShow l == T.intercalate (T.singleton '\n') (map (uncurry eqMerge ) $ historySimplify l)

    describe "statToJSONText" $ do

      it "returns a valid dice JSON in the Text datatype" $ forAll arbitraryStats $ verifyStats

    describe "historyUpdate_" $ do

      it "correctly updates a history" $ forAll (replicateM 2 arbitraryHistory) $
        \[h1, h2] -> addHistories (h1, h2) == reverse h2 ++ h1

    describe "explodingRoll" $ do

      it "does not explode normal dice" $ forAll (arbitraryIndefiniteRoll False) $
        \(res, face, test) -> Right res == evalState (explodingRoll res face test) (pureMT 1, [], HM.empty)


      it "properly explodes exploding dice, and identifies infinitely exploding dice" $ forAll (arbitraryIndefiniteRoll True) $
        \(res, face, test) -> case evalState (explodingRoll res face test) (pureMT 1, [], HM.empty) of
                                Left _ -> infiniteTest face test
                                Right l -> length l == length res + countPred (toComparator test) l

    describe "rerollRoll" $ do

      it "does not reroll normal dice" $ forAll (arbitraryIndefiniteRoll False) $
        \(res, face, test) -> Right res == evalState (explodingRoll res face test) (pureMT 1, [], HM.empty)


      it "properly rerolls rerolling dice, and identifies infinitely rerolling dice" $ forAll (arbitraryIndefiniteRoll True) $
        \(res, face, test) -> case evalState (rerollRoll res face test) (pureMT 1, [], HM.empty) of
                                Left _ -> infiniteTest face test
                                Right l -> length res == countKept l && checkDropped test l

    describe "keepDropResolve" $ do

      it "does not change dice without a keep/drop rule" $ forAll (arbitraryKDTest False) $
        \(possL, kd) -> keepDropResolve possL kd == possL

      it "properly keeps or drops the correct dice" $ forAll (arbitraryKDTest True) $
        \(possL, mkd) -> let res = keepDropResolve possL mkd
                             origCount = countKept possL
                             keptCount = countKept res
                        in origCount == keptCount + countRemoved mkd possL

    describe "successResolve" $ do

      it "sums the faces of non-success dice" $ forAll (arbitrarySuccess False) $
        \(possL, test) -> successResolve possL test == sum (filterDropped possL)

      it "properly counts success dice" $ forAll (arbitrarySuccess True) $
        \(possL, test) -> successResolve possL test == filterTest test possL
