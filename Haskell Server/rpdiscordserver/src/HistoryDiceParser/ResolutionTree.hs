{-# LANGUAGE RankNTypes #-}
module HistoryDiceParser.ResolutionTree
(OpStatic(..)
,OpType(..)
,OpNode(..)
,FunRes(..)
,UnType(..)
,FunType(..)
,nodeShow
,treeResolve
,resShow) where

import           Control.Monad.ST
import           Data.Char
import qualified Data.HashMap.Strict           as HM
import           Data.List
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Text.Lazy                as T
import           Data.Word
import           System.Random.Mersenne.Pure64

import           Debug.Trace

import           General.UserNumber
import           HistoryDiceParser.Operators

simpleIntRange :: Int -> Int -> PureMT -> (Int, PureMT)
simpleIntRange mini maxi gen
  |word < maxRange = (fromInteger $ reverseRange (word `mod` fromInteger wordRange) mini, nGen)
  |otherwise = simpleIntRange mini maxi nGen
  where
    wordRange :: Integer
    wordRange = fromIntegral maxi - fromIntegral mini
    (word, nGen) = randomWord64 gen
    maxWord = maxBound :: Word64
    maxRange = maxWord - (maxWord `mod` fromInteger wordRange)
    reverseRange :: Word64 -> Int -> Integer
    reverseRange word int = fromIntegral word + fromIntegral int

randomFace ::  Int -> Int -> PureMT -> ([GeneralNumber], PureMT)
randomFace 1 face gen
  |(int, nGen) <- simpleIntRange 1 (face+1) gen = ([GReal $ GSimp $ GInt $ fromIntegral int], nGen)
randomFace n face gen
  |(int, nGen) <- simpleIntRange 1 (face+1) gen, (list, lGen) <- randomFace (n-1) face nGen = ((GReal $ GSimp $ GInt $ fromIntegral int):list, lGen)

randomListSelect ::  Int -> PureMT -> [a] -> ([a], PureMT)
randomListSelect 1 gen list
  |(int, nGen) <- simpleIntRange 0 (length list) gen = ([list !! int], nGen)
randomListSelect n gen list
  |(int, nGen) <- simpleIntRange 0 (length list) gen, (rList, lGen) <- randomListSelect (n-1) nGen list = ((list !! int):rList, lGen)

poolRoll :: Integral a => a -> Either Int [GeneralNumber] -> PureMT -> ([GeneralNumber], PureMT)
poolRoll n (Left side) generator = randomFace (fromIntegral n) side generator
poolRoll n (Right list) generator = randomListSelect (fromIntegral n) generator list

dStatsAdd :: HM.HashMap (Word, Word) (HM.HashMap Word Word) -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> HM.HashMap (Word, Word) (HM.HashMap Word Word)
dStatsAdd = HM.unionWith $ HM.unionWith (+)

{-data NumTest = TestNone | TestLeq GeneralRealNumber | TestLes GeneralRealNumber | TestGeq GeneralRealNumber | TestGre GeneralRealNumber
                 | TestEq [GeneralNumber] | TestNeq [GeneralNumber] | TestIn GeneralRealNumber GeneralRealNumber | TestOut GeneralRealNumber GeneralRealNumber
data Dice = Die {poolSize::      Int,
                 poolDis::       String,
                 face::          Either Int [GeneralNumber],
                 faceDis::       String,
                 exploding::     (NumTest, Bool), -- If True, display number, else hide it
                 reroll::        NumTest,
                 keep_drop::     Maybe Int,
                 keep_drop_type::Maybe KeepDrop,
                 success::       NumTest}-}

diceRoll :: Dice -> PureMT -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> Either ((T.Text, GeneralNumber), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException
diceRoll d@Die {poolSize=pool, face=faceposs, exploding=explodtest, reroll=rerolltest, keep_drop=maybekd, keep_drop_type=maybekdt, success=successtest, faceDis=facestring} generator diceMap
  |infTest faceposs $ fst explodtest = Right $ ResolveException $ T.pack "Infinitely exploding roll detected. Roll aborted."
  |infTest faceposs rerolltest = Right $ ResolveException $ T.pack "Infinitely rerolling roll detected. Roll aborted."
  |otherwise = traceShow rerolltest Left $ (endTotal . resKD . resReroll) (resSplode res resMap)
  where
    inRange :: GeneralRealNumber -> GeneralRealNumber -> GeneralRealNumber -> Bool
    inRange mini maxi x = mini <= x && x <= maxi
    filterSides :: Int -> [GeneralNumber] -> [GeneralNumber]
    filterSides _ [] = []
    filterSides facei (x:xs)
      |GReal n <- x, x `notElem` xs && 1 <= n && n <= fromIntegral facei = x:filterSides facei xs
      |otherwise = filterSides facei xs
    orderedTest :: (GeneralRealNumber -> Bool) -> GeneralNumber -> Bool
    orderedTest test x
      |GReal n <- x = test n
      |otherwise = False
    infTest :: Either Int [GeneralNumber] -> NumTest -> Bool
    infTest (Left facei) numTest
      |TestNone <- numTest     = False
      |TestLeq n <- numTest    = n >= fromIntegral facei
      |TestLes n <- numTest    = n > fromIntegral facei
      |TestGeq n <- numTest    = n <= 1
      |TestGre n <- numTest    = n < 1
      |TestEq list <- numTest  = length (filterSides facei list) >= facei
      |TestNeq list <- numTest = not $ null (filterSides facei list)
      |TestIn m n <- numTest   = m <= 1 && fromIntegral facei <= n
      |TestOut m n <- numTest  = m > fromIntegral facei || 1 > n
    infTest (Right numList) numTest
      |TestNone <- numTest     = False
      |TestLeq n <- numTest    = any (orderedTest $ (>=) n) numList
      |TestLes n <- numTest    = any (orderedTest $ (>) n)  numList
      |TestGeq n <- numTest    = any (orderedTest $ (<=) n) numList
      |TestGre n <- numTest    = any (orderedTest $ (<) n) numList
      |TestEq list <- numTest  = any (`notElem` list) numList
      |TestNeq list <- numTest = any (`elem` list) numList
      |TestIn m n <- numTest   = not $ all (orderedTest $ inRange m n) numList
      |TestOut m n <- numTest  = any (orderedTest $ inRange m n) numList
    res = poolRoll pool faceposs generator
    resMap = checkMap pool facestring (fst res) diceMap
    smartInc :: Maybe Word -> Maybe Word
    smartInc Nothing  = Just 1
    smartInc (Just v) = Just $ v + 1
    smartMerge :: Word -> Maybe (HM.HashMap Word Word) -> Maybe (HM.HashMap Word Word)
    smartMerge n Nothing        = Just $ HM.singleton n 1
    smartMerge n (Just wordMap) = Just $ HM.alter smartInc n wordMap
    checkMap :: Int -> String -> [GeneralNumber] -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> HM.HashMap (Word, Word) (HM.HashMap Word Word)
    checkMap ps text numbers miniMap
      |all isDigit text, GReal (GSimp (GInt n)) <- intsum = HM.alter (smartMerge $ fromIntegral n) (fromIntegral ps, read text) miniMap
      |otherwise = miniMap
      where
        intsum = sum numbers
    emptyTestPred :: NumTest -> Bool
    emptyTestPred TestNone = True
    emptyTestPred _ = False
    testOrListEl :: NumTest -> GeneralNumber -> Int
    testOrListEl numTest x
      |TestNone <- numTest                               = 0
      |TestLeq m <- numTest, GReal n <- x, n <= m        = 1
      |TestLeq m <- numTest                              = 0
      |TestLes m <- numTest, GReal n <- x, n <  m        = 1
      |TestLes m <- numTest                              = 0
      |TestGeq m <- numTest, GReal n <- x, n >= m        = 1
      |TestGeq m <- numTest                              = 0
      |TestGre m <- numTest, GReal n <- x, n >  m        = 1
      |TestGre m <- numTest                              = 0
      |TestEq list <- numTest, x `elem` list             = 1
      |TestEq list <- numTest                            = 0
      |TestNeq list <- numTest, x `notElem` list         = 1
      |TestNeq list <- numTest                           = 0
      |TestIn m n <- numTest, GReal p <- x, m<=p && p<=n = 1
      |TestIn m n <- numTest                             = 0
      |TestOut m n <- numTest, GReal p <- x, m>p || p>n  = 1
      |TestOut m n <- numTest                            = 0
    resSplode :: ([GeneralNumber], PureMT) -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> ([GeneralNumber], PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word))
    resSplode (results, gen) miniMap
      |explodeNum == 0 = (results, gen, miniMap)
      |(newRes, nGen) <- poolRoll explodeNum faceposs gen, (restRes, lGen, _) <- resSplode (newRes, nGen) miniMap = (results ++ restRes, lGen, miniMap)
      where
        explodeNum = foldl (\a b -> a + testOrListEl (fst $ traceShowId explodtest) b) 0 results  --length (filter (\x -> elem x explodlist) results)
    resReroll :: ([GeneralNumber], PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) -> ([PossNumber], PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word))
    resReroll (results, gen, miniMap)
      |count == 0 = (posslist, gen, miniMap)
      |(newRes, nGen) <- poolRoll count faceposs gen, (restRes, lGen, _) <- resReroll (newRes, nGen, miniMap) = (posslist ++ restRes, lGen, miniMap)
      where
        ignoreRes :: [GeneralNumber] -> ([PossNumber], Integer)
        ignoreRes [] = ([], 0)
        ignoreRes (r:rs)
          |testOrListEl rerolltest r == 1 = (Dropped r:poss, count + 1)
          |otherwise = (Kept r:poss, count)
          where
            (poss, count) = ignoreRes rs
        (posslist, count) = ignoreRes results
    resKD :: ([PossNumber], PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) -> ([PossNumber], PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word))
    resKD (results, gen, miniMap)
      |Just kd <- maybekd, Just kdt <- maybekdt = (fullMake results kdt kd, gen, miniMap)
      |otherwise = (results, gen, miniMap)
      where
        indListR :: [PossNumber] -> Int -> ([(GeneralRealNumber, Int)], [(PossNumber, Int)])
        indListR [] n = ([], [])
        indListR (x:xs) n
          |Kept (GReal y) <- x, (list1, list2) <- indListR xs (n+1) = ((y, n):list1, (x, n):list2)
          |(list1, list2) <- indListR xs (n+1) = (list1, (x, n):list2)
        getSecond :: (a, b) -> b
        getSecond (a, b) = b
        eleGrab :: [(GeneralRealNumber, Int)] -> KeepDrop -> Int -> [Int]
        eleGrab list KeepHigh n = genericDrop (genericLength list - n) $ map getSecond list
        eleGrab list DropHigh n = genericTake (genericLength list - n) $ map getSecond list
        eleGrab list KeepLow n  = genericTake n                          $ map getSecond list
        eleGrab list DropLow n  = genericDrop n                          $ map getSecond list
        eleSelect :: [(PossNumber, Int)] -> [Int] -> [PossNumber]
        eleSelect [] indices = []
        eleSelect ((x, ind):xs) indices
          |Kept y <- x, ind `elem` indices = Kept y:eleSelect xs indices
          |Kept y <- x = Dropped y:eleSelect xs indices
          |otherwise = x:eleSelect xs indices
        fullMake :: [PossNumber] -> KeepDrop -> Int -> [PossNumber]
        fullMake results kdt kd = endList
          where
            (indexedGenList, indexedPossList) = indListR results 0
            sortedlist = sortBy (\(a, _) (b, _) -> compare a b) indexedGenList
            keptInd = eleGrab sortedlist kdt kd
            endList = eleSelect indexedPossList keptInd
    endTotal :: ([PossNumber], PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) -> ((T.Text, GeneralNumber), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word))
    endTotal (results, gen, miniMap)
      |emptyTestPred successtest = (smartSum results, gen, miniMap)
      |otherwise = (smartSuccess results, gen, miniMap)
      where
        stringify :: [PossNumber] -> T.Text
        stringify numList = T.concat $ head strList : map plusForPos (tail strList)
          where
            strList = map (T.pack . show) numList
            plusForPos :: T.Text -> T.Text
            plusForPos numStr
              |T.singleton '-' `T.isPrefixOf` numStr = numStr
              |otherwise = T.cons '+' numStr
        sumNumsH :: PossNumber -> GeneralNumber
        sumNumsH (Kept n) = n
        sumNumsH x        = 0
        sumNums :: [PossNumber] -> GeneralNumber
        sumNums list = sum $ map sumNumsH list
        smartSum :: [PossNumber] -> (T.Text, GeneralNumber)
        smartSum list = (T.concat [T.singleton '(', stringify list, T.singleton ')', T.pack $ showDieSuffix d], sumNums list)
        succNumsH :: PossNumber -> GeneralNumber
        succNumsH (Kept n) = GReal $ GSimp $ GInt $ fromIntegral $ testOrListEl successtest n
        succNumsH x = 0
        succNums :: [PossNumber] -> GeneralNumber
        succNums list = sum $ map succNumsH list
        smartSuccess :: [PossNumber] -> (T.Text, GeneralNumber)
        smartSuccess list = (T.concat [T.singleton '(', T.intercalate (T.singleton ',') (map (T.pack . show) list), T.singleton ')', T.pack $ showDieSuffix d], traceShowId $ succNums list)

data VecText = Flat T.Text | Deep [VecText]
vecTextShow :: VecText -> T.Text
vecTextShow (Flat t) = t
vecTextShow (Deep tlist) = T.concat [T.singleton '(', T.intercalate (T.singleton ',') (map vecTextShow tlist), T.singleton ')']

vecRoll :: OpVector -> PureMT -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> Either ((VecText, OpVector), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException
vecRoll (OpVector list) gen diceMap = finisher $ rollVecR list gen diceMap
  where
    rollVecR :: [OpStatic] -> PureMT -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> Either (([VecText], [OpStatic]), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException
    rollVecR [] gen miniMap = Left (([], []), gen, miniMap)
    rollVecR (x:xs) gen miniMap
      |StaticDie d <- x, Right e <- diceRoll d gen miniMap = Right e
      |StaticDie d <- x, Left ((string, number), nGen, newMap) <- diceRoll d gen miniMap, Right e <- rollVecR xs nGen newMap = Right e
      |StaticDie d <- x,
       Left ((string, number), nGen, newMap) <- diceRoll d gen miniMap,
       Left ((restStr, restNum), lGen, lastMap) <- rollVecR xs nGen newMap = Left ((Flat string:restStr, StaticNum number:restNum), lGen, lastMap)
      |StaticVec v <- x, Right e <- vecRoll v gen miniMap = Right e
      |StaticVec v <- x, Left ((string, vector), nGen, newMap) <- vecRoll v gen miniMap, Right e <- rollVecR xs nGen newMap = Right e
      |StaticVec v <- x,
      Left ((string, vector), nGen, newMap) <- vecRoll v gen miniMap,
      Left ((restStr, restNum), lGen, lastMap) <- rollVecR xs nGen newMap = Left ((string:restStr, StaticVec vector:restNum), lGen, lastMap)
      |StaticNum n <- x, Right e <- rollVecR xs gen miniMap = Right e
      |StaticNum n <- x,
      Left ((restStr, restNum), lGen, newMap) <- rollVecR xs gen miniMap = Left (((Flat $ T.pack $ show n):restStr, x:restNum), lGen, newMap)
      |StaticBool b <- x, Right e <- rollVecR xs gen miniMap = Right e
      |StaticBool b <- x,
      Left ((restStr, restNum), lGen, newMap) <- rollVecR xs gen miniMap = Left (((Flat $ T.pack $ show b):restStr, x:restNum), lGen, newMap)
    finisher :: Either (([VecText], [OpStatic]), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException -> Either ((VecText, OpVector), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException
    finisher (Left ((text, num), gen, miniMap)) = Left ((Deep text, OpVector num), gen, miniMap)
    finisher (Right e) = Right e

--data OpStatic = StaticDie Dice | StaticNum GeneralNumber | StaticVec OpVector | StaticBool Bool
--OpVector [OpStatic]
{-{poolSize::      Integer,
                     poolDis::       String,
                     face::          [GeneralNumber],
                     faceDis::       String,
                     exploding::     [GeneralNumber],
                     reroll::        [GeneralNumber],
                     keep_drop::     Maybe Integer,
                     keep_drop_type::Maybe KeepDrop,
                     success::       [GeneralNumber],
                     suffix::        String,
                     constant::      GeneralNumber}-}

extract (Left (TypeNode t,_)) = t

functionWithChildren :: OpNode -> [T.Text] -> T.Text
functionWithChildren OpNode {nodeDisplay = test, nodeFunctionKey = Nothing} childDis
  |test == T.pack "res" = T.concat [T.singleton '[', T.intercalate (T.singleton ',') childDis, T.singleton ']']
  |test == T.pack "vec" = T.concat [T.singleton '(', T.intercalate (T.singleton ',') childDis, T.singleton ')']
functionWithChildren OpNode {nodeDisplay = dis, nodeFunction = fun, nodeChildren = children} childDis
  |FUnary _ prec Prefix  <- fun, [x] <- children, [s] <- childDis = T.append dis $ disTest prefPred (Just prec) (Just x) s
  |FUnary _ prec Suffix  <- fun, [x] <- children, [s] <- childDis = T.append (disTest suffPred (Just prec) (Just x) s) dis
  |FInfix _ prec <- fun, [x,y] <- children, [s,t] <- childDis = T.concat [disTest suffPred (Just prec) (Just x) s, dis, disTest prefPred (Just prec) (Just y) t]
  |FFunct _ <- fun = T.append dis $ disTest prefPred Nothing Nothing $ T.intercalate (T.singleton ',') childDis
  where
    getPrec :: OpType -> Maybe Integer
    getPrec (TypeNode OpNode {nodeFunction = fun})
      |FUnary _ prec _ <- fun = Just prec
      |FInfix _ prec <- fun = Just prec
    getPrec x = Nothing
    prefPred :: T.Text -> Bool
    prefPred text
      |Just (c, _) <- T.uncons text, c `elem` ['(', '['] = True
      |otherwise = False
    suffPred :: T.Text -> Bool
    suffPred text
      |Just (_, c) <- T.unsnoc text, c `elem` [')', ']'] = True
      |otherwise = False
    disTest :: (T.Text -> Bool) -> Maybe Integer -> Maybe OpType -> T.Text -> T.Text
    disTest fun (Just prec) (Just child) childDis
      |TypeStatic n <- child = childDis
      |TypeStatic n <- child = childDis
      |Just childPrec <- getPrec child, prec < childPrec = childDis
      |fun childDis = childDis
      |otherwise = T.concat [T.singleton '(', childDis, T.singleton ')']
    disTest fun Nothing Nothing childDis
      |fun childDis = childDis
      |otherwise = T.concat [T.singleton '(', childDis, T.singleton ')']

nodeShow :: OpNode -> T.Text
nodeShow node
  |OpNode {nodeChildren = children} <- node = functionWithChildren node $ map displayChild children
    where
      displayChild :: OpType -> T.Text
      displayChild (TypeNode childNode)        = nodeShow childNode
      displayChild (TypeStatic (StaticDie d))  = T.pack $ show d
      displayChild (TypeStatic (StaticNum n))  = T.pack $ show n
      displayChild (TypeStatic (StaticVec v))  = T.pack $ show v
      displayChild (TypeStatic (StaticBool b)) = T.pack $ show b

sillySplit :: PureMT -> (PureMT, PureMT)
sillySplit gen = (sGen, pureMT word)
  where
    (word, sGen) = randomWord64 gen

historyShow :: [(T.Text, T.Text)] -> T.Text
historyShow [] = T.empty
historyShow tupList = T.intercalate (T.singleton '\n') $ map tupShow tupList
  where
    tupShow :: (T.Text, T.Text) -> T.Text
    tupShow (before, after) = T.concat [before, T.singleton '=', after]

{-
vecRoll  :: OpVector -> PureMT -> Either ((VecText, OpVector), PureMT) ResolveException
diceRoll :: Dice -> PureMT -> Either ((T.Text, GeneralNumber), PureMT) ResolveException

data FunType = FUnary (OpType -> FunRes) Integer UnType | FInfix (OpType -> OpType -> FunRes) Integer | FFunct ([OpType] -> FunRes) | FPossUn  (OpType -> OpType -> FunRes) Integer (OpType -> FunRes) Integer UnType
data OpNode = OpNode {nodeDisplay:: T.Text, nodeFunctionKey::Maybe T.Text, nodeResOrder :: OpResolveOrder, nodeFunction::FunType, nodeChildren::[OpType]}
data OpResolveOrder = OpResolveLeft | OpResolveRight | OpResolveAll
data OpToken = OpToken {optype :: OpTokenType, resolveOrder :: OpResolveOrder, assoc :: Assoc, function :: FunType}
data OpStatic = StaticDie Dice | StaticNum GeneralNumber | StaticVec OpVector | StaticBool Bool
data OpType = TypeStatic OpStatic | TypeNode OpNode
-}

diceStatShow :: [((Word, Word), [(Word, Word)])] -> T.Text
diceStatShow diceStat = T.concat [T.singleton '{', T.intercalate (T.singleton ',') $ map highMap diceStat, T.singleton '}']
  where
    highMap :: ((Word, Word), [(Word, Word)]) -> T.Text
    highMap ((pool, die), value) = T.append (T.pack $ "\"" ++ show pool ++ "d" ++ show die ++ "\":") mappedValue
      where
        mappedValue = T.concat [T.singleton '{', T.intercalate (T.singleton ',') $ map lowMap value, T.singleton '}']
    lowMap :: (Word, Word) -> T.Text
    lowMap (result, count) = T.pack ("\"" ++ show result ++ "\":" ++ show count)

resShow :: Either (T.Text, HM.HashMap (Word, Word) (HM.HashMap Word Word)) (Either ParseException ResolveException) -> T.Text
resShow (Left (t, diceStat)) = T.concat [T.pack "Roll Statistics:\n", diceText, T.pack "\nResults:\n", t]
  where
    diceText = diceStatShow $ HM.toList $ HM.map HM.toList diceStat
resShow (Right (Left e)) = T.pack $ show e
resShow (Right (Right e)) = T.pack $ show e

treeResolve :: Either (OpType, PureMT) ParseException -> Either (T.Text, HM.HashMap (Word, Word) (HM.HashMap Word Word)) (Either ParseException ResolveException)
treeResolve (Right e) = Right $ Left e
treeResolve (Left (root, generator))
  |(Errored e, _, _) <- res = Right $ Right e
  |(Resolved (TypeStatic (StaticNum n)), history, lastMap) <- res = Left (historyShow history, lastMap)
  |(Resolved (TypeStatic (StaticBool b)), history, lastMap) <- res= Left (historyShow history, lastMap)
  |(Resolved (TypeStatic (StaticDie d)), history, lastMap) <- res = finalDieRoll d lGen history lastMap
  |(Resolved (TypeStatic (StaticVec v)), history, lastMap) <- res = finalVecRoll v lGen history lastMap
  where
    (nGen, lGen) = sillySplit generator
    emptyMap = HM.empty
    firstResolve :: OpType -> PureMT -> (FunRes, [(T.Text, T.Text)], HM.HashMap (Word, Word) (HM.HashMap Word Word))
    firstResolve (TypeNode n) gen = nodeResolve (n, gen, emptyMap) []
    firstResolve r@(TypeStatic (StaticNum n)) _ = (Resolved r, [(T.pack $ show n, T.pack $ show n)], emptyMap)
    firstResolve r@(TypeStatic (StaticBool b)) _ = (Resolved r, [(T.pack $ show b, T.pack $ show b)], emptyMap)
    firstResolve x _ = (Resolved x, [], emptyMap)
    res = firstResolve root nGen
    finalDieRoll :: Dice -> PureMT -> [(T.Text, T.Text)] -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> Either (T.Text, HM.HashMap (Word, Word) (HM.HashMap Word Word)) (Either ParseException ResolveException)
    finalDieRoll d gen history diceMap = finisher $ diceRoll d gen diceMap
      where
        finisher :: Either ((T.Text, GeneralNumber), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException -> Either (T.Text, HM.HashMap (Word, Word) (HM.HashMap Word Word)) (Either ParseException ResolveException)
        finisher (Right e) = Right $ Right e
        finisher (Left ((text, num), _, lastMap)) = Left (historyShow $ snoc history (text, T.pack $ show num), lastMap)
    finalVecRoll :: OpVector -> PureMT -> [(T.Text, T.Text)] -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> Either (T.Text, HM.HashMap (Word, Word) (HM.HashMap Word Word)) (Either ParseException ResolveException)
    finalVecRoll v gen history diceMap = finisher $ vecRoll v gen diceMap
      where
        finisher :: Either ((VecText, OpVector), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException -> Either (T.Text, HM.HashMap (Word, Word) (HM.HashMap Word Word)) (Either ParseException ResolveException)
        finisher (Right e) = Right $ Right e
        finisher (Left ((vecText, vecNum), _, lastMap)) = Left (historyShow $ snoc history (vecTextShow vecText, T.pack $ show vecNum), lastMap)

resolutionFun :: FunType -> [OpType] -> FunRes
resolutionFun (FUnary fun _ _) [x] = fun x
resolutionFun (FInfix fun _) [x,y] = fun x y
resolutionFun (FFunct fun) list    = fun list

nodeResolve :: (OpNode, PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) -> [(T.Text, T.Text)] -> (FunRes, [(T.Text, T.Text)], HM.HashMap (Word, Word) (HM.HashMap Word Word))
nodeResolve (baseNode@OpNode {nodeFunction = fun, nodeChildren = children, nodeResOrder = resOrder}, baseGen, origDice) preHistory
  |Right e <- bigResults = (Errored e, [], origDice)
  |Left (Just (newNode, newGen, newHistory, newMap)) <- orderCheck resOrder baseGen origDice children = nodeResolve (newNode, newGen, newMap) newHistory
  |Right e <- orderCheck resOrder baseGen origDice children = (Errored e, [], origDice)
  |Left (resChildList, newGen, nextDice) <- bigResults = rootResolve fun resOrder resChildList newGen nextDice
  where
    orderCheck :: OpResolveOrder -> PureMT -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> [OpType]
                  -> Either (Maybe (OpNode, PureMT, [(T.Text, T.Text)], HM.HashMap (Word, Word) (HM.HashMap Word Word))) ResolveException
    orderCheck OpResolveLeftFirst gen diceMap (TypeNode x:xs)
      |Errored e <- res = Right e
      |Resolved r <- res, Errored e <- resolutionFun fun $ r:xs = Right e
      |Resolved r <- res,
       NeedsRolls <- resolutionFun fun $ r:xs,
       (ncGen, nnGen) <- sillySplit nGen,
       Right e <- childRoll (r, history) ncGen newMap = Right e
      |Resolved r <- res,
       NeedsRolls <- resolutionFun fun $ r:xs,
       (ncGen, nnGen) <- sillySplit nGen,
       Left ((nr, nextHistory), newnewMap) <- childRoll (r, history) ncGen newMap,
       Resolved (TypeNode n) <- resolutionFun fun $ nr:xs = Left $ Just (n, nnGen, nextHistory, newnewMap)
      |Resolved r <- res, Resolved (TypeNode n) <- resolutionFun fun $ r:xs = Left $ Just (n, nGen, history, newMap)
      where
        (cGen, nGen) = sillySplit gen
        (res, history, newMap) = nodeResolve (x, cGen, diceMap) preHistory
    orderCheck OpResolveLeftFirst gen diceMap list
      |Errored e <- resolutionFun fun list = Right e
      |Resolved (TypeNode n) <- resolutionFun fun list = Left $ Just (n, gen, preHistory ++ [(nodeShow baseNode, nodeShow n)], diceMap)
    orderCheck OpResolveRightFirst gen diceMap list
      |Just (Errored e, _, _) <- possRes = Right e
      |Just (Resolved r, _, _) <- possRes, Errored e <- resolutionFun fun $ snoc xs r = Right e
      |Just (Resolved r, history, newMap) <- possRes,
       NeedsRolls <- resolutionFun fun $ snoc xs r,
       (ncGen, nnGen) <- sillySplit nGen,
       Right e <- childRoll (r, history) ncGen newMap = Right e
      |Just (Resolved r, history, newMap) <- possRes,
       NeedsRolls <- resolutionFun fun $ snoc xs r,
       (ncGen, nnGen) <- sillySplit nGen,
       Left ((nr, nextHistory), newnewMap) <- childRoll (r, history) ncGen newMap,
       Resolved (TypeNode n) <- resolutionFun fun $ snoc xs nr = Left $ Just (n, nnGen, nextHistory, newnewMap)
      |Just (Resolved r, history, newMap) <- possRes,
       Resolved (TypeNode n) <- resolutionFun fun $ snoc xs r = Left $ Just (n, nGen, history, newMap)
      |Nothing <- possRes, Errored e <- resolutionFun fun list = Right e
      |Nothing <- possRes, Resolved (TypeNode n) <- resolutionFun fun list = Left $ Just (n, gen, preHistory ++ [(nodeShow baseNode, nodeShow n)], diceMap)
      where
        (cGen, nGen) = sillySplit gen
        (xs, le) = fromJust $ unsnoc list
        restResolve :: OpType -> Maybe (FunRes, [(T.Text, T.Text)], HM.HashMap (Word, Word) (HM.HashMap Word Word))
        restResolve (TypeNode n) = Just $ nodeResolve (n, cGen, diceMap) preHistory
        restResolve n = Nothing
        possRes = restResolve le
    orderCheck _ _ _ _ = Left Nothing
    resolveChild :: (OpType, PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) -> (FunRes, [(T.Text, T.Text)], HM.HashMap (Word, Word) (HM.HashMap Word Word))
    resolveChild (TypeStatic (StaticNum n), _, diceMap) = (Resolved $ TypeStatic $ StaticNum n,  [(T.pack $ show n, T.pack $ show n)], diceMap)
    resolveChild (TypeStatic (StaticBool b), _, diceMap) = (Resolved $ TypeStatic $ StaticBool b, [(T.pack $ show b, T.pack $ show b)], diceMap)
    resolveChild (TypeNode childNode, gen, diceMap) = nodeResolve (childNode, gen, diceMap) []
    threadChildren :: ([OpType], PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) -> Either ([(OpType, [(T.Text, T.Text) ])], PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException
    threadChildren ([], gen, diceMap) = Left ([], gen, diceMap)
    threadChildren (x:xs, gen, diceMap)
      |(Errored e, _, _) <- childRes = Right e
      |(Resolved c, list, childDice) <- childRes, Left (restRes, lGen, restDice) <- threadChildren (xs, nGen, diceMap)= Left ((c, list):restRes, lGen, dStatsAdd childDice restDice)
      where
        (cGen, nGen) = sillySplit gen
        childRes = resolveChild (x, cGen, HM.empty)
    bigResults = threadChildren (children, baseGen, origDice)
    makeMyList :: [(OpType, [(T.Text, T.Text)])] -> T.Text -> [(T.Text, T.Text)]
    makeMyList tupList lastText = finisher lastText preHistory $ goingDown 0 tupList []
      where
        goingDown :: Int -> [(OpType, [(T.Text, T.Text)])] -> [(Int, [(T.Text, T.Text)])] -> [([T.Text], [T.Text])]
        goingDown n [] passedList = goingUp n passedList $ replicate n ([],[])
        goingDown n ((_, list):xs) passedList = goingDown (max n $ length list) xs $ (length list, list):passedList
        goingUp :: Int -> [(Int, [(T.Text, T.Text)])] -> [([T.Text],[T.Text])] -> [([T.Text],[T.Text])]
        goingUp _ [] passedList = passedList
        goingUp maxLength ((curLength, curList):xs) passedList = goingUp maxLength xs $ riffle (curList ++ replicate (maxLength-curLength) (last curList)) passedList
          where
            riffle :: [(T.Text, T.Text)] -> [([T.Text],[T.Text])] -> [([T.Text],[T.Text])]
            riffle [] [] = []
            riffle ((preText, postText):ns) ((preList, postList):os) = (preText:preList, postText:postList):riffle ns os
        finisherR :: T.Text -> [([T.Text],[T.Text])] -> [(T.Text,T.Text)]
        finisherR _ [] = []
        finisherR text [(x1, _)] = [(functionWithChildren baseNode x1, text)]
        --Figure out source of duplicate at some point
        finisherR text ((x1, x2):xs)
          |e1 == e2 = finisherR text xs
          |otherwise = (e1, e2):finisherR text xs
          where
            e1 = functionWithChildren baseNode x1
            e2 = functionWithChildren baseNode x2
        finisher :: T.Text -> [(T.Text, T.Text)] -> [([T.Text],[T.Text])] -> [(T.Text,T.Text)]
        finisher text []   []        = []
        finisher text hist []        = snoc (init hist) (fst $ last hist, text)
        finisher text hist childlist = hist ++ finisherR text childlist
    childRoll :: (OpType, [(T.Text, T.Text)]) -> PureMT -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> Either ((OpType, [(T.Text, T.Text)]), HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException
    childRoll (singleResult, textHistory) gen diceMap
     |TypeStatic (StaticDie d) <- singleResult = diceParser (diceRoll d gen diceMap) textHistory
     |TypeStatic (StaticVec v)<- singleResult = vecParser (vecRoll v gen diceMap) textHistory
     |otherwise = Left ((singleResult, textHistory), diceMap)
      where
        diceParser :: Either ((T.Text, GeneralNumber), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException -> [(T.Text, T.Text)] -> Either ((OpType, [(T.Text, T.Text)]), HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException
        diceParser (Right e) _ = Right e
        diceParser (Left ((resolveText, number), _, nextMap)) oldHistory = Left ((TypeStatic $ StaticNum number, snoc oldHistory (resolveText, T.pack $ show number)), nextMap)
        vecParser  :: Either ((VecText, OpVector), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException -> [(T.Text, T.Text)] -> Either ((OpType, [(T.Text, T.Text)]), HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException
        vecParser (Right e) _ = Right e
        vecParser (Left ((textVec, resVec), _, nextMap)) oldHistory = Left ((TypeStatic $ StaticVec resVec, snoc oldHistory (vecTextShow textVec, T.pack $ show resVec)), nextMap)
    rootResolve :: FunType -> OpResolveOrder -> [(OpType, [(T.Text, T.Text)])] -> PureMT -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> (FunRes, [(T.Text, T.Text)], HM.HashMap (Word, Word) (HM.HashMap Word Word))
    rootResolve fun rOrd childList gen diceMap
      |NeedsRolls <- res,
       OpResolveLeft <- rOrd,
       Just (firstChild, restChild) <- uncons childList,
       Right e <- childRoll firstChild gen diceMap = (Errored e, [], diceMap)
      |NeedsRolls <- res,
       OpResolveLeft <- rOrd,
       Just (firstChild, restChild) <- uncons childList,
       Left (success, nextMap) <- childRoll firstChild gen diceMap = resolveAgain fun (cons success restChild) nextMap
      |NeedsRolls <- res,
       OpResolveRight <- rOrd,
       Just (mostChild, lastChild) <- unsnoc childList,
       Right e <- childRoll lastChild gen diceMap = (Errored e, [], diceMap)
      |NeedsRolls <- res,
       OpResolveRight <- rOrd,
       Just (mostChild, lastChild) <- unsnoc childList,
       Left (success, nextMap) <- childRoll lastChild gen diceMap = resolveAgain fun (snoc mostChild success) nextMap
      |NeedsRolls <- res, Right e <- childThreadRoll childList gen diceMap                                                                  = (Errored e, [], diceMap)
      |NeedsRolls <- res, Left (success, nextDice) <- childThreadRoll childList gen diceMap                                                 = resolveAgain fun success nextDice
      |Errored e <- res                                                                                                                     = (res, [], diceMap)
      |Resolved (TypeNode OpNode {nodeDisplay = nd, nodeFunctionKey = Nothing, nodeFunction = FFunct vecFunction, nodeChildren = newchildren}) <- res,
       Resolved (TypeStatic r) <- vecFunction newchildren, nd == T.pack "vec"                                                               = (Resolved (TypeStatic r), makeMyList childList $ T.pack $ show r, diceMap)
      |Resolved (TypeStatic r) <- res                                                                                                       = (res, makeMyList childList $ T.pack $ show r, diceMap)
      where
        childResults = map fst childList
        res = resolutionFun fun childResults
        --(cGen, nGen) = sillySplit gen
        childThreadRoll :: [(OpType, [(T.Text, T.Text)])] -> PureMT -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> Either ([(OpType, [(T.Text, T.Text)])], HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException
        childThreadRoll [] gen diceMap = Left ([], diceMap)
        childThreadRoll (x:xs) gen diceMap
          |Right e <- oneRes = Right e
          |Right e <- restRes = Right e
          |Left (res, smallMap) <- oneRes, Left (list, bigMap) <- restRes = Left (res:list, dStatsAdd smallMap bigMap)
          where
            (gcGen, ncGen) = sillySplit gen
            oneRes = childRoll x gcGen HM.empty
            restRes = childThreadRoll xs ncGen diceMap
        resolveAgain :: FunType -> [(OpType, [(T.Text, T.Text)])] -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> (FunRes, [(T.Text, T.Text)], HM.HashMap (Word, Word) (HM.HashMap Word Word))
        resolveAgain resFun rollChildren diceMap
          |NeedsRolls <- sndRes              = traceShow rollChildren (Errored $ ResolveException $ T.pack "Double Roll at one node detected, which shouldn't happen. Further investigation is needed.", [], diceMap)
          |Errored e <- sndRes               = (sndRes, [], diceMap)
          |Resolved (TypeStatic r) <- sndRes = (sndRes, makeMyList rollChildren $ T.pack $ show r, diceMap)
          where
            sndRes = resolutionFun resFun $ map fst rollChildren

--diceRoll :: Dice -> PureMT -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> Either ((T.Text, GeneralNumber), PureMT, HM.HashMap (Word, Word) (HM.HashMap Word Word)) ResolveException
--dStatsAdd :: HM.HashMap (Word, Word) (HM.HashMap Word Word) -> HM.HashMap (Word, Word) (HM.HashMap Word Word) -> HM.HashMap (Word, Word) (HM.HashMap Word Word)
