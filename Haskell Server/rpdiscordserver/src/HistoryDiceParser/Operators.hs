module HistoryDiceParser.Operators
(KeepDrop(..)
,smartIntegerToInt
,NumTest(..)
,Dice(..)
,createDie
,showDieSuffix
,addExplode
,addReroll
,addKeepDrop
,addSuccess
,OpVector(..)
,OpStatic(..)
,OpType(..)
,FunRes(..)
,UnType(..)
,FunType(..)
,OpResolveOrder(..)
,OpNode(..)
,operatorKeys
,operatorDict
,OpTokenType(..)
,OpToken(..)
,getType
,Assoc(..)
,opVector
,opResVector
,vectorify) where

import qualified Data.Text.Lazy as T
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Maybe

import Debug.Trace

import General.UserNumber

data KeepDrop = KeepHigh | DropHigh | KeepLow | DropLow
  deriving (Eq)

data NumTest = TestNone | TestLeq GeneralRealNumber | TestLes GeneralRealNumber | TestGeq GeneralRealNumber | TestGre GeneralRealNumber
                 | TestEq [GeneralNumber] | TestNeq [GeneralNumber] | TestIn GeneralRealNumber GeneralRealNumber | TestOut GeneralRealNumber GeneralRealNumber
  deriving (Show, Eq)

smartIntegerToInt :: Integer -> Int
smartIntegerToInt n
  |n > fromIntegral (maxBound::Int) = maxBound::Int
  |n < fromIntegral (minBound::Int) = minBound::Int
  |otherwise = fromInteger n

data Dice = Die {poolSize::      Int,
                 poolDis::       String,
                 face::          Either Int [GeneralNumber],
                 faceDis::       String,
                 exploding::     (NumTest, Bool), -- If True, display number, else hide it
                 reroll::        NumTest,
                 keep_drop::     Maybe Int,
                 keep_drop_type::Maybe KeepDrop,
                 success::       NumTest}
  deriving (Eq)

createDie :: Int -> String -> Either Int [GeneralNumber] -> String -> Dice
createDie poolnum poolstr facerep facestring =
          Die {poolSize=poolnum,
              poolDis=poolstr,
              face=facerep,
              faceDis=facestring,
              exploding=(TestNone, False),
              reroll=TestNone,
              keep_drop=Nothing,
              keep_drop_type=Nothing,
              success=TestNone}

addExplode :: Dice -> NumTest -> Bool -> Dice
addExplode die@Die {face=faceposs} numTest bool = die {exploding = (numTest, bool)}

addReroll :: Dice -> NumTest -> Dice
addReroll die@Die {face=faceposs} numTest = die {reroll = numTest}

addKeepDrop :: Dice -> KeepDrop -> Int -> Dice
addKeepDrop die kdt num = die {keep_drop=Just num, keep_drop_type=Just kdt}
  where
    dis :: KeepDrop -> String
    dis KeepHigh = "kh"
    dis KeepLow = "kl"
    dis DropHigh = "dh"
    dis DropLow = "dl"

addSuccess :: Dice -> NumTest -> Dice
addSuccess die@Die {face=faceposs} numTest = die {success = numTest}

showDieSuffix :: Dice -> String
showDieSuffix Die {exploding = (explodingTest, explodingBool), reroll = rerollTest, keep_drop=kd, keep_drop_type=kdt, success=successTest} = suffix
  where
    fancyListShow :: [GeneralNumber] -> String
    fancyListShow [] = "()"
    fancyListShow [x] = show x
    fancyListShow list = "(" ++ intercalate ", " (map show list) ++ ")"
    parseGenericTest :: String -> NumTest -> String
    parseGenericTest pre TestNone = ""
    parseGenericTest pre (TestLeq n) = pre ++ "<=" ++ show (GReal n)
    parseGenericTest pre (TestLes n) = pre ++ "<" ++ show (GReal n)
    parseGenericTest pre (TestGeq n) = pre ++ ">=" ++ show (GReal n)
    parseGenericTest pre (TestGre n) = pre ++ ">" ++ show (GReal n)
    parseGenericTest pre (TestEq n)  = (if pre == "" then "==" else pre) ++ fancyListShow n
    parseGenericTest pre (TestNeq n) = pre ++ "/=" ++ fancyListShow n
    parseGenericTest pre (TestIn m n) = pre ++ "In(" ++ show (GReal m) ++ "," ++ show (GReal n) ++ ")"
    parseGenericTest pre (TestOut m n) = pre ++ "Out(" ++ show (GReal m) ++ "," ++ show (GReal n) ++ ")"
    parseKeepDrop :: Maybe Int -> Maybe KeepDrop -> String
    parseKeepDrop Nothing Nothing = ""
    parseKeepDrop (Just n) (Just KeepHigh) = "kh" ++ show n
    parseKeepDrop (Just n) (Just DropHigh) = "dh" ++ show n
    parseKeepDrop (Just n) (Just KeepLow)  = "kl" ++ show n
    parseKeepDrop (Just n) (Just DropLow)  = "dl" ++ show n
    suffix = concat $ filter ("" /=) [if explodingBool then parseGenericTest "!" explodingTest else (if explodingTest /= TestNone then "!" else ""),
                                      parseGenericTest "r" rerollTest, parseKeepDrop kd kdt,
                                      parseGenericTest "" successTest]

instance Show Dice where
  show d@Die {poolDis=pool, faceDis=fac} = pool ++ "d" ++ fac ++ showDieSuffix d

newtype OpVector = OpVector [OpStatic]
  deriving (Eq)

vecMap f (OpVector list) = OpVector $ map f list

instance Show OpVector where
  show (OpVector list) = "(" ++ intercalate "," (map show list) ++ ")"

strDup :: (Integral a) => String -> a -> String
strDup str n = concat $ replicate (fromIntegral n) str

data InvalidNumberComp = InvalidNumberComp String String

data OpStatic = StaticDie Dice | StaticNum GeneralNumber | StaticVec OpVector | StaticBool Bool
  deriving (Eq)

instance Show OpStatic where
  show (StaticDie d) = show d
  show (StaticNum n) = numShow n
  show (StaticVec v) = show v
  show (StaticBool b) = show b

data OpType = TypeStatic OpStatic | TypeNode OpNode
  deriving (Eq)

instance Show OpType where
  show (TypeStatic o) = show o
  show (TypeNode o) = show o

data FunRes = NeedsRolls | Resolved OpType | Errored ResolveException
  deriving Show

data UnType = Prefix | Suffix
  deriving Show

data FunType = FUnary (OpType -> FunRes) Integer UnType | FInfix (OpType -> OpType -> FunRes) Integer | FFunct ([OpType] -> FunRes) | FPossUn  (OpType -> OpType -> FunRes) Integer (OpType -> FunRes) Integer UnType

instance Show FunType where
  show (FUnary _ int t) = "(FUnary fun " ++ show int ++ " " ++ show t ++ ")"
  show (FInfix _ int) = "(FInfix fun " ++ show int ++ ")"
  show (FFunct _ ) = "(FFunct fun)"
  show (FPossUn _ int1 _ int2 t) = "(FPossUn fun1 " ++ show int1 ++ " fun2 " ++ show int2 ++ " " ++ show t ++")"

data OpResolveOrder = OpResolveLeft | OpResolveRight | OpResolveAll | OpResolveLeftFirst | OpResolveRightFirst
  deriving Show
data OpNode = OpNode {nodeDisplay :: T.Text , nodeFunctionKey ::Maybe T.Text, nodeResOrder :: OpResolveOrder, nodeFunction :: FunType, nodeChildren :: [OpType]}

instance Eq OpNode where
  OpNode {nodeDisplay = nd1, nodeFunctionKey = key1, nodeChildren = child1} == OpNode {nodeDisplay = nd2, nodeFunctionKey = key2, nodeChildren = child2}
    = (nd1==nd1) && (key1==key2) && (child1 == child2)

instance Show OpNode where
  show x = indShow x 0
    where
      indShow :: OpNode -> Integer -> String
      indShow OpNode {nodeDisplay= dis, nodeChildren=child} n = strDup "\t" n ++ T.unpack dis ++ "\n" ++ intercalate "\n" (map smartShow child)
        where
          smartShow :: OpType -> String
          smartShow ele
            |TypeNode x <- ele = indShow x (n+1)
            |TypeStatic x <- ele = strDup "\t" (n+1) ++ show x

maybeCons :: a -> Maybe [a] -> Maybe [a]
maybeCons a Nothing = Nothing
maybeCons a (Just as) = Just (a:as)

baseVecTest :: (OpStatic -> Maybe a) -> OpVector -> Maybe [a]
baseVecTest matcher (OpVector list) = baseVecTestH matcher list
  where
    baseVecTestH :: (OpStatic -> Maybe a) -> [OpStatic] -> Maybe [a]
    baseVecTestH matcher [] = Just []
    baseVecTestH matcher (x:xs)
      |Just e <- matcher x = maybeCons e (baseVecTestH matcher xs)
      |otherwise = Nothing

vecPosIntTest :: OpVector -> Maybe [Integer]
vecPosIntTest = baseVecTest posIntTest
  where
    posIntTest :: OpStatic -> Maybe Integer
    posIntTest (StaticNum (GReal (GSimp (GInt n)))) = if n >=0 then Just n else Nothing
    posIntTest _ = Nothing

vecIntTest :: OpVector -> Maybe [Integer]
vecIntTest = baseVecTest intTest
  where
    intTest :: OpStatic -> Maybe Integer
    intTest (StaticNum (GReal (GSimp (GInt n)))) = Just n
    intTest _ = Nothing


vecRealNumTest :: OpVector -> Maybe [GeneralRealNumber]
vecRealNumTest = baseVecTest numTest
  where
    numTest :: OpStatic -> Maybe GeneralRealNumber
    numTest (StaticNum n)
      |GReal m <- n = Just m
      |otherwise = Nothing
    numTest _ = Nothing

vecNumTest :: OpVector -> Maybe [GeneralNumber]
vecNumTest = baseVecTest numTest
  where
    numTest :: OpStatic -> Maybe GeneralNumber
    numTest (StaticNum n) = Just n
    numTest _ = Nothing

vecRealNumOrDieTest :: OpVector -> Maybe [OpStatic]
vecRealNumOrDieTest = baseVecTest realNumOrDieTest
  where
    realNumOrDieTest :: OpStatic -> Maybe OpStatic
    realNumOrDieTest n@(StaticNum (GReal _)) = Just n
    realNumOrDieTest d@(StaticDie Die {face = facePoss})
      |Left _ <- facePoss = Just d
      |Right list <- facePoss, all realTest list = Just d
      |otherwise = Nothing
    realNumOrDieTest _ = Nothing
    realTest :: GeneralNumber -> Bool
    realTest (GReal _) = True
    realTest _ = False

childIntTest :: [OpType] -> Maybe [Integer]
childIntTest [] = Just []
childIntTest (x:xs)
  |TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- x = maybeCons n $ childIntTest xs
  |otherwise = Nothing

childNumTest :: [OpType] -> Maybe [GeneralNumber]
childNumTest [] = Just []
childNumTest (x:xs)
  |TypeStatic (StaticNum num) <- x = maybeCons num $ childNumTest xs
  |otherwise = Nothing

childSingleStaticTest :: [OpType] -> Maybe [Either GeneralNumber Dice]
childSingleStaticTest [] = Just []
childSingleStaticTest (x:xs)
  |TypeStatic (StaticNum num) <- x = maybeCons (Left num) $ childSingleStaticTest xs
  |TypeStatic (StaticDie d) <- x = maybeCons (Right d) $ childSingleStaticTest xs
  |otherwise = Nothing

childStaticTest :: [OpType] -> Maybe [OpStatic]
childStaticTest [] = Just []
childStaticTest (x:xs)
  |TypeStatic ele <- x = maybeCons ele $ childStaticTest xs
  |otherwise = Nothing

realDieTest :: Dice -> Maybe (Either Int [GeneralRealNumber])
realDieTest Die {face=Left n} = Just $ Left n
realDieTest Die {face=Right facelist} = isreal facelist
  where
    maybeEither :: GeneralRealNumber -> Maybe (Either Int [GeneralRealNumber]) -> Maybe (Either Int [GeneralRealNumber])
    maybeEither x Nothing = Nothing
    maybeEither x (Just (Right xs)) = Just $ Right $ x:xs
    isreal :: [GeneralNumber] -> Maybe (Either Int [GeneralRealNumber])
    isreal [] = Just $ Right []
    isreal (GReal x:xs) = maybeEither x $ isreal xs
    isreal list = Nothing

feedThrough :: (Show a) => (String -> a -> b) -> (a -> b)
feedThrough fun a = fun (show a) a

dFunction :: String -> OpType -> OpType -> FunRes
dFunction string (TypeStatic (StaticNum num)) op2
  |GReal (GSimp (GInt m)) <- num, m>0, TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- op2, n > 0
    = moldres $ Left $ createDie (smartIntegerToInt m) string (Left $ smartIntegerToInt n) (show $ smartIntegerToInt n)
  |GReal (GSimp (GInt m)) <- num, m>0, TypeStatic (StaticNum n) <- op2 = Errored $ ResolveException $ T.pack "Non-positive number passed as right argument to d."
  |GReal (GSimp (GInt m)) <- num, m>0, TypeStatic (StaticVec (OpVector [])) <- op2 = Errored $ ResolveException $ T.pack "Empty list passed as right argument to d."
  |GReal (GSimp (GInt m)) <- num, m>0, TypeStatic (StaticVec vec) <- op2, Just v <- vecNumTest vec
    = moldres $ Left $ createDie (smartIntegerToInt m) string (Right v) (show vec)
  |GReal (GSimp (GInt m)) <- num, m>0 = NeedsRolls
  |GReal (GSimp (GInt m)) <- num = Errored $ ResolveException $ T.pack "Non-positive number passed as left argument to d."
  |otherwise = Errored $ ResolveException $ T.pack "Non-integer number passed as left argument to d."
  where
    moldres :: Either Dice ResolveException -> FunRes
    moldres (Left x) = Resolved $ TypeStatic $ StaticDie x
    moldres (Right y) = Errored y

dFunction _ (TypeStatic (StaticVec vec)) op2 = Errored $ ResolveException $ T.pack "Vector passed as left argument to d."
dFunction _ (TypeStatic (StaticBool b)) op2 = Errored $ ResolveException $ T.pack "Boolean passed as left argument to d."
dFunction _ _ _ = NeedsRolls

vectorify :: [OpType] -> FunRes
vectorify list
  |Nothing <- test = NeedsRolls
  |Just x <- test = Resolved $ TypeStatic $ StaticVec $ OpVector x
  where
    test = childStaticTest list

dupFunction :: OpType -> OpType -> FunRes
dupFunction (TypeStatic (StaticNum num)) op2
  |GReal (GSimp (GInt n)) <- num, n >= 0 = Resolved $ TypeNode OpNode {nodeDisplay = T.pack "vec", nodeFunctionKey = Nothing, nodeResOrder = OpResolveAll, nodeFunction=FFunct vectorify, nodeChildren= replicate (fromInteger (min n 50)) op2}
  |GReal (GSimp (GInt n)) <- num = Errored $ ResolveException $ T.pack "Negative integer was passed as left argument to #."
  |otherwise = Errored $ ResolveException $ T.pack "Non-integer was passed as left argument to #."
dupFunction (TypeStatic (StaticVec (OpVector list))) op2 = finisher $ vecParse list op2
  where
    smartCons :: Either OpType (Either () ResolveException) -> Either [OpType] (Either () ResolveException) -> Either [OpType] (Either () ResolveException)
    smartCons (Right (Right e)) list = Right $ Right e
    smartCons ele (Right (Right e)) = Right $ Right e
    smartCons (Right (Left ())) list = Right $ Left ()
    smartCons ele (Right (Left ())) = Right $ Left ()
    smartCons (Left ele) (Left list) = Left (ele:list)
    toNode :: [OpType] -> OpNode
    toNode list = OpNode {nodeDisplay = T.pack "vec", nodeFunctionKey = Nothing, nodeResOrder = OpResolveAll, nodeFunction=FFunct vectorify,  nodeChildren=list}
    numParse :: GeneralNumber -> OpType -> Either OpType (Either () ResolveException)
    numParse (GReal (GSimp (GInt n))) op
      |n >= 0 = Left $ TypeNode OpNode {nodeDisplay = T.pack "vec", nodeFunctionKey = Nothing, nodeResOrder = OpResolveAll, nodeFunction=FFunct vectorify, nodeChildren=replicate (fromInteger n) op}
      |otherwise = Right $ Right $ ResolveException $ T.pack "Negative integer was passed in vector left argument to #."
    numParse num op = Right $ Right $ ResolveException $ T.pack "Non-integer was passed in vector left argument to #."
    vecParse :: [OpStatic] -> OpType -> Either [OpType] (Either () ResolveException)
    vecParse [] op = Left []
    vecParse list@(StaticDie d:xs) op = Right $ Left ()
    vecParse list@(StaticNum num:xs) op = smartCons (numParse num op) (vecParse xs op)
    vecParse list@(StaticVec (OpVector vec):xs) op
      |Left list <- res = smartCons (Left $ TypeNode $ toNode list) (vecParse xs op)
      |Right (Left ()) <- res = Right $ Left ()
      |Right (Right e) <- res = Right $ Right e
      where
        res = vecParse vec op
    finisher :: Either [OpType] (Either () ResolveException) -> FunRes
    finisher (Left list) = Resolved $ TypeNode OpNode {nodeDisplay = T.pack "vec", nodeFunctionKey = Nothing, nodeResOrder = OpResolveAll, nodeFunction=FFunct vectorify, nodeChildren=list}
    finisher (Right (Left ())) = NeedsRolls
    finisher (Right (Right e)) = Errored e
dupFunction (TypeStatic (StaticBool b)) op2 = Errored $ ResolveException $ T.pack "Boolean passed as left argument to #"
dupFunction op1 op2 = NeedsRolls

fudgeDie :: OpType -> FunRes
fudgeDie (TypeStatic (StaticNum num))
  |GReal (GSimp (GInt n)) <- num, n > 0 = moldres $ Left $ createDie (smartIntegerToInt n) (show $ smartIntegerToInt n) (Right fudgelist) "F"
  |GReal (GSimp (GInt n)) <- num = Errored $ ResolveException $ T.pack "Non-positive integer passed to dF."
  |otherwise = Errored $ ResolveException $ T.pack "Non-integer passed to dF."
  where
    fudgelist = [-1, 0, 1]
    moldres :: Either Dice ResolveException -> FunRes
    moldres (Left x) = Resolved $ TypeStatic $ StaticDie x
    moldres (Right y) = Errored y
fudgeDie (TypeStatic (StaticVec vec)) = Errored $ ResolveException $ T.pack "Vector passed as argument to dF."
fudgeDie (TypeStatic (StaticBool b)) = Errored $ ResolveException $ T.pack "Boolean passed as argument to dF."
fudgeDie op = NeedsRolls

--vecNumTest :: OpVector -> Maybe [GeneralNumber]
--vecRealNumOrDieTest :: OpVector -> Maybe [OpStatic]
--data KeepDrop = KeepHigh | DropHigh | KeepLow | DropLow
--data OpVector = OpVector [OpStatic]
--vecRealNumOrDieTest :: OpVector -> Maybe [OpStatic]
--vecRealNumTest :: OpVector -> Maybe [GeneralNumber]

smartGet :: KeepDrop -> Int -> [GeneralRealNumber] -> OpStatic
smartGet kd n list
  |kd == KeepHigh = generalComp (\(_, a) (_, b) -> compare b a) take
  |kd == DropHigh = generalComp (\(_, a) (_, b) -> compare b a) drop 
  |kd == KeepLow = generalComp (\(_, a) (_, b) -> compare a b) take
  |kd == DropLow = generalComp (\(_, a) (_, b) -> compare a b) drop 
  where
    generalComp :: ((Int, GeneralRealNumber) -> (Int, GeneralRealNumber) -> Ordering) -> (Int -> [(Int, GeneralRealNumber)] -> [(Int, GeneralRealNumber)]) -> OpStatic 
    generalComp sortingFun siftFun = StaticVec $ OpVector $ map (\a -> StaticNum $ GReal a) $ map (\(_, a) -> a) $ sortBy (\(a, _) (b, _) -> compare a b) $ siftFun n $ sortBy sortingFun $ snd $ mapAccumL (\ind element -> (ind+1, (ind, element))) 0 list

keepdropFun :: KeepDrop -> OpType -> OpType -> FunRes
keepdropFun kd op1 op2
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- op2, n >=0 = Resolved $ TypeStatic $ StaticDie $ addKeepDrop d kd $ smartIntegerToInt n
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticNum (GReal (GSimp (GInt _)))) <- op2 = Errored $ ResolveException $ T.pack "Negative number passed as right argument to keep/drop function."
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticNum _) <- op2 = Errored $ ResolveException $ T.pack "Non-integer number passed as right argument to keep/drop function."
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticDie _) <- op2 = NeedsRolls 
  |TypeStatic (StaticDie d) <- op1, TypeStatic _ <- op2 = Errored $ ResolveException $ T.pack "Non-number passed as right argument to keep/drop function."
  |TypeStatic (StaticDie d) <- op1 = NeedsRolls
  |TypeStatic (StaticVec v) <- op1, Just list <- vecRealNumTest v, TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- op2, n >=0 = Resolved $ TypeStatic $ smartGet kd (smartIntegerToInt n) list 
  |TypeStatic (StaticVec v) <- op1, Just _ <- vecRealNumOrDieTest v, TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- op2, n >=0 = NeedsRolls 
  |TypeStatic (StaticVec v) <- op1, TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- op2, n >=0 = Errored $ ResolveException $ T.pack "Vector with non-real entries passed as left argument to keep/drop function." 
  |TypeStatic (StaticVec v) <- op1, TypeStatic (StaticNum (GReal (GSimp (GInt _)))) <- op2 = Errored $ ResolveException $ T.pack "Negative number passed as right argument to keep/drop function."
  |TypeStatic (StaticVec v) <- op1, TypeStatic (StaticNum _) <- op2 = Errored $ ResolveException $ T.pack "Non-integer number passed as right argument to keep/drop function."
  |TypeStatic (StaticVec v) <- op1, TypeStatic (StaticDie _) <- op2 = NeedsRolls 
  |TypeStatic (StaticDie d) <- op1, TypeStatic _ <- op2 = Errored $ ResolveException $ T.pack "Non-number passed as right argument to keep/drop function."
  |TypeStatic x <- op1 = Errored $ ResolveException $ T.pack "Non-die passed as left argument to keep/drop function."
  |otherwise = NeedsRolls

parseNumTestToken :: NumTest -> OpStatic -> Either NumTest (Either () ResolveException)
parseNumTestToken TestNone _ = Left TestNone
parseNumTestToken (TestLeq _) arg
  |StaticNum (GReal n) <- arg = Left $ TestLeq n
  |StaticNum n <- arg = Right $ Right $ ResolveException $ T.pack "Unordered number passed to less than or equal to dice comparison."
  |StaticDie d <- arg = Right $ Left ()
  |otherwise = Right $ Right $ ResolveException $ T.pack "Non-number passed to less than or equal to dice comparison"
parseNumTestToken (TestLes _) arg
  |StaticNum (GReal n) <- arg = Left $ TestLes n
  |StaticNum n <- arg = Right $ Right $ ResolveException $ T.pack "Unordered number passed to less than dice comparison."
  |StaticDie d <- arg = Right $ Left ()
  |otherwise = Right $ Right $ ResolveException $ T.pack "Non-number passed to less than dice comparison"
parseNumTestToken (TestGeq _) arg
  |StaticNum (GReal n) <- arg = Left $ TestGeq n
  |StaticNum n <- arg = Right $ Right $ ResolveException $ T.pack "Unordered number passed to greater than or equal to dice comparison."
  |StaticDie d <- arg = Right $ Left ()
  |otherwise = Right $ Right $ ResolveException $ T.pack "Non-number passed to greater than or equal to dice comparison"
parseNumTestToken (TestGre _) arg
  |StaticNum (GReal n) <- arg = Left $ TestGre n
  |StaticNum n <- arg = Right $ Right $ ResolveException $ T.pack "Unordered number passed to greater than dice comparison."
  |StaticDie d <- arg = Right $ Left ()
  |otherwise = Right $ Right $ ResolveException $ T.pack "Non-number passed to greater than dice comparison"
parseNumTestToken (TestEq _) arg
  |StaticNum n <- arg = Left $ TestEq [n]
  |StaticDie d <- arg = Right $ Left ()
  |StaticVec v <- arg, Just l <- vecNumTest v = Left $ TestNeq l
  |StaticVec v <- arg, Just _ <- vecRealNumOrDieTest v = Right $ Left ()
  |otherwise = Right $ Right $ ResolveException $ T.pack "Non-number/numeric vector passed to equal to dice comparison"
parseNumTestToken (TestNeq _) arg
  |StaticNum n <- arg = Left $ TestNeq [n]
  |StaticDie d <- arg = Right $ Left ()
  |StaticVec v <- arg, Just l <- vecNumTest v = Left $ TestNeq l
  |StaticVec v <- arg, Just _ <- vecRealNumOrDieTest v = Right $ Left ()
  |otherwise = Right $ Right $ ResolveException $ T.pack "Non-number/numeric vector passed to not equal to dice comparison"
parseNumTestToken (TestIn _ _) arg
  |StaticVec (OpVector [StaticNum (GReal m), StaticNum (GReal n)]) <- arg = Left $ TestIn m n
  |StaticVec (OpVector [StaticNum _, StaticNum _]) <- arg = Right $ Right $ ResolveException $ T.pack "Vector with non-ordered elements passed to dice range comparison."
  |StaticVec (OpVector [StaticDie _, StaticDie _]) <- arg = Right $ Left ()
  |StaticVec vec@(OpVector _) <- arg, Just l <- vecNumTest vec = Right $ Right $ ResolveException $ T.pack "Vector of incorrect length passed to dice range comparison."
  |StaticVec (OpVector _) <- arg = Right $ Right $ ResolveException $ T.pack "Non-numeric vector passed to dice range comparison."
  |otherwise = Right $ Right $ ResolveException $ T.pack "Non-vector passed to dice range comparison"
parseNumTestToken (TestOut _ _) arg
  |StaticVec (OpVector [StaticNum (GReal m), StaticNum (GReal n)]) <- arg = Left $ TestOut m n
  |StaticVec (OpVector [StaticNum _, StaticNum _]) <- arg = Right $ Right $ ResolveException $ T.pack "Vector with non-ordered elements passed to dice range comparison."
  |StaticVec (OpVector [StaticDie _, StaticDie _]) <- arg = Right $ Left ()
  |StaticVec vec@(OpVector _) <- arg, Just l <- vecNumTest vec = Right $ Right $ ResolveException $ T.pack "Vector of incorrect length passed to dice range comparison."
  |StaticVec (OpVector _) <- arg = Right $ Right $ ResolveException $ T.pack "Non-numeric vector passed to dice range comparison."
  |otherwise = Right $ Right $ ResolveException $ T.pack "Non-vector passed to dice range comparison"

rerollFun :: NumTest -> OpType -> OpType -> FunRes
rerollFun numTest op1 op2
  |TypeStatic (StaticDie d) <- op1, TypeStatic x <- op2 = resParse d x
  |TypeStatic (StaticDie d) <- op1 = Errored $ ResolveException $ T.pack "Non-number passed as right argument to reroll function."
  |TypeStatic x <- op1 = Errored $ ResolveException $ T.pack "Non-die passed as left argument to reroll function."
  |otherwise = NeedsRolls
  where
    resParse :: Dice -> OpStatic -> FunRes
    resParse d x
      |Left test <- res = Resolved $ TypeStatic $ StaticDie $ addReroll d test
      |Right (Left _) <- res = NeedsRolls
      |Right (Right e) <- res = Errored e
      where
        res = parseNumTestToken numTest x

exclamationpoint :: OpType -> FunRes
exclamationpoint (TypeStatic (StaticDie d)) = maxExplode d
  where
    maxExplode :: Dice -> FunRes
    maxExplode d
      |Just (Left faceint) <- res = Resolved $ TypeStatic $ StaticDie $ addExplode d (TestEq [fromIntegral faceint]) False
      |Just (Right facelist) <- res = Resolved $ TypeStatic $ StaticDie $ addExplode d (TestEq [GReal $ maximum facelist]) False
      |otherwise = Errored $ ResolveException $ T.pack "Die with non-ordered faces passed to exploding function."
      where
        res = realDieTest d
exclamationpoint (TypeStatic (StaticNum n)) = genFactorial n
  where
    factorial :: Integer -> Integer
    factorial 0 = 1
    factorial n = n * factorial (n-1)
    genFactorial :: GeneralNumber -> FunRes
    genFactorial (GReal (GSimp (GInt n)))
      |n >= 0 = Resolved $ TypeStatic $ StaticNum $ GReal $ GSimp $ GInt $ factorial n
      |otherwise = Errored $ ResolveException $ T.pack "Negative Integer passed to !"
    genFactorial x = Errored $ ResolveException $ T.pack "Non-integer number passed to !"
exclamationpoint (TypeStatic (StaticVec n)) = Errored $ ResolveException $ T.pack "Vector passed to !."
exclamationpoint (TypeStatic (StaticBool b)) = Errored $ ResolveException $ T.pack "Boolean passed to !."
exclamationpoint x = NeedsRolls

explodingFun :: NumTest -> OpType -> OpType -> FunRes
explodingFun numTest (TypeStatic (StaticDie d)) (TypeStatic x) = resParse d x
  where
    resParse :: Dice -> OpStatic -> FunRes
    resParse d x
      |Left test <- res = Resolved $ TypeStatic $ StaticDie $ addExplode d test True
      |Right (Left _) <- res = NeedsRolls
      |Right (Right e) <- res = Errored e
      where
        res = parseNumTestToken numTest x
explodingFun _ (TypeStatic x) _ = Errored $ ResolveException $ T.pack "Non-die passed to left of exploding function."
explodingFun _ _ _ = NeedsRolls

toBool :: OpStatic -> Bool
toBool (StaticBool x) = x
toBool (StaticNum 0) = False
toBool (StaticNum x) = True
toBool (StaticVec (OpVector [])) = False
toBool (StaticVec x) = True

opNot :: OpType -> FunRes
opNot (TypeStatic (StaticDie x)) = NeedsRolls
opNot (TypeStatic x) = Resolved $ TypeStatic $ StaticBool $ not $ toBool x
opNot a = NeedsRolls

opNotFun :: [OpType] -> FunRes
opNotFun [x] = opNot x

opExp :: OpType -> OpType -> FunRes
opExp (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Resolved $ TypeStatic $ StaticNum $ genExp x y
opExp (TypeStatic (StaticVec x)) b = Errored $ ResolveException $ T.pack "Vector passed as left argument to exponentiation."
opExp a (TypeStatic (StaticVec y)) = Errored $ ResolveException $ T.pack "Vector passed as right argument to exponentiation."
opExp (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed as left argument to exponentiation."
opExp a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed as right argument to exponentiation."
opExp a b = NeedsRolls

smartVecMult :: GeneralNumber -> OpVector -> FunRes
smartVecMult x (OpVector list) = smartListMult x list
  where
    smartCons :: Either OpStatic (Either [OpStatic] (Maybe ResolveException)) -> Either [OpStatic] (Maybe ResolveException) -> Either [OpStatic] (Maybe ResolveException)
    smartCons (Left n) rest
      |Left list <- rest = Left (n:list)
      |otherwise = rest
    smartCons (Right l) rest
      |Left list <- l = smartCons (Left $ StaticVec $ OpVector list) rest
      |otherwise = l
    checkRolls :: [OpStatic] -> Maybe ResolveException
    checkRolls [] = Nothing
    checkRolls (x:xs)
      |StaticBool b <- x = Just $ ResolveException $ T.pack "Vector containing Bool passed as argument to multiplication."
      |otherwise = checkRolls xs
    smartListMultR :: GeneralNumber -> [OpStatic] -> Either [OpStatic] (Maybe ResolveException)
    smartListMultR n [] = Left []
    smartListMultR n (StaticNum x:xs) = smartCons (Left $ StaticNum $ n*x) $ smartListMultR n xs
    smartListMultR n (StaticBool b:xs) = Right $ Just $ ResolveException $ T.pack "Vector containing Bool passed as argument to multiplication."
    smartListMultR n (StaticVec (OpVector list):xs) = smartCons (Right $ smartListMultR n list) $ smartListMultR n xs
    smartListMultR n (x:xs) = Right $ checkRolls xs
    finisher :: Either [OpStatic] (Maybe ResolveException) -> FunRes
    finisher (Left list) = Resolved $ TypeStatic $ StaticVec $ OpVector list
    finisher (Right Nothing) = NeedsRolls
    finisher (Right (Just e)) = Errored e
    smartListMult :: GeneralNumber -> [OpStatic] -> FunRes
    smartListMult n list = finisher $ smartListMultR n list
smartDotMult :: OpVector -> OpVector -> FunRes
smartDotMult (OpVector list1) (OpVector list2) = smartListMult list1 list2
  where
    smartCons :: OpStatic -> Either [OpStatic] (Maybe ResolveException) -> Either [OpStatic] (Maybe ResolveException)
    smartCons n (Left list) = Left (n:list)
    smartCons n (Right x) = Right x
    checkRolls :: [OpStatic] -> [OpStatic] -> Maybe ResolveException
    checkRolls [] [] = Nothing
    checkRolls (x:xs) (y:ys)
      |StaticVec v <- x = Just $ ResolveException $ T.pack "Vector with non-integer components passed to vector multiplication."
      |StaticVec v <- y = Just $ ResolveException $ T.pack "Vector with non-integer components passed to vector multiplication."
      |StaticBool b <- x = Just $ ResolveException $ T.pack "Vector with non-integer components passed to vector multiplication."
      |StaticBool b <- y = Just $ ResolveException $ T.pack "Vector with non-integer components passed to vector multiplication."
      |otherwise = checkRolls xs ys
    smartListMultR :: [OpStatic] -> [OpStatic] -> Either [OpStatic] (Maybe ResolveException)
    smartListMultR [] [] = Left []
    smartListMultR (x:xs) (y:ys)
      |StaticNum n <- x, StaticNum m <- y = smartCons (StaticNum $ n*m) $ smartListMultR xs ys
      |StaticVec v <- x = Right $ Just $ ResolveException $ T.pack "Vector with non-integer components passed to vector multiplication."
      |StaticVec v <- y = Right $ Just $ ResolveException $ T.pack "Vector with non-integer components passed to vector multiplication."
      |StaticBool b <- x = Right $ Just $ ResolveException $ T.pack "Vector with non-integer components passed to vector multiplication."
      |StaticBool b <- y = Right $ Just $ ResolveException $ T.pack "Vector with non-integer components passed to vector multiplication."
      |otherwise = Right $ checkRolls xs ys
    finisher :: Either [OpStatic] (Maybe ResolveException) -> FunRes
    finisher (Left list) = Resolved $ TypeStatic $ StaticVec $ OpVector list
    finisher (Right Nothing) = NeedsRolls
    finisher (Right (Just e)) = Errored e
    smartListMult :: [OpStatic] -> [OpStatic] -> FunRes
    smartListMult list1 list2
      |length list1 == length list2 = finisher $ smartListMultR list1 list2
      |otherwise = Errored $ ResolveException $ T.pack "Vectors of different lengths passed as arguments to multiplication"
opMult :: OpType -> OpType -> FunRes
opMult (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed as left argument to multiplication."
opMult a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed as right argument to multiplication."
opMult (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Resolved $ TypeStatic $ StaticNum $ x * y
opMult (TypeStatic (StaticVec v)) (TypeStatic (StaticNum x)) = smartVecMult x v
opMult (TypeStatic (StaticNum x)) (TypeStatic (StaticVec v)) = smartVecMult x v
opMult (TypeStatic (StaticVec v1)) (TypeStatic (StaticVec v2)) = smartDotMult v1 v2
opMult a b = NeedsRolls

opDiv :: OpType -> OpType -> FunRes
opDiv (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed as left argument to division."
opDiv a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed as right argument to division."
opDiv (TypeStatic (StaticVec x)) b = Errored $ ResolveException $ T.pack "Vector passed as left argument to division."
opDiv a (TypeStatic (StaticVec y)) = Errored $ ResolveException $ T.pack "Vector passed as right argument to division."
opDiv (TypeStatic (StaticNum x)) (TypeStatic (StaticNum 0)) = Errored $ ResolveException $ T.pack "Division by Zero Error."
opDiv (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Resolved $ TypeStatic $ StaticNum $ x / y
opDiv a b = NeedsRolls

opMod :: OpType -> OpType -> FunRes
opMod (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed as left argument to modulus."
opMod a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed as right argument to modulus."
opMod (TypeStatic (StaticVec x)) b = Errored $ ResolveException $ T.pack "Vector passed as left argument to modulus."
opMod a (TypeStatic (StaticVec y)) = Errored $ ResolveException $ T.pack "Vector passed as right argument to modulus."
opMod (TypeStatic (StaticNum (GReal x))) (TypeStatic (StaticNum (GReal y))) = Resolved $ TypeStatic $ StaticNum $ GReal $ realMod x y
opMod (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Complex number passed as argument to modulus."
opMod a b = NeedsRolls

opAnd :: OpType -> OpType -> FunRes
opAnd (TypeStatic (StaticDie x)) b = NeedsRolls
opAnd a (TypeStatic (StaticDie y)) = NeedsRolls
opAnd (TypeStatic x) (TypeStatic y) = Resolved $ TypeStatic $ StaticBool $ toBool x && toBool y
opAnd a b = NeedsRolls

vecAdd :: OpVector -> OpVector -> FunRes
vecAdd (OpVector list1) (OpVector list2) = listAdd list1 list2
  where
    smartCons :: Either OpStatic (Either [OpStatic] (Maybe ResolveException)) -> Either [OpStatic] (Maybe ResolveException) -> Either [OpStatic] (Maybe ResolveException)
    smartCons (Left x) rest
      |Left list <- rest = Left (x:list)
      |Right Nothing <- rest = Right Nothing
      |Right (Just e) <- rest = Right $ Just e
    smartCons (Right x) rest
      |Left list <- x = smartCons (Left $ StaticVec $ OpVector list) rest
      |Right Nothing <- x = Right Nothing
      |Right (Just e) <- x = Right $ Just e
    listAddH :: [OpStatic] -> [OpStatic] -> Either [OpStatic] (Maybe ResolveException)
    listAddH [] [] = Left []
    listAddH (x:xs) (y:ys)
      |StaticBool b <- x = Right $ Just $ ResolveException $ T.pack "Vector containing bool passed to addition."
      |StaticBool b <- y = Right $ Just $ ResolveException $ T.pack "Vector containing bool passed to addition."
      |StaticNum n <- x, StaticNum m <- y = smartCons (Left $ StaticNum $ n+m) (listAddH xs ys)
      |StaticVec (OpVector v1) <- x, StaticVec (OpVector v2) <- y = smartCons (Right $ listAddH v1 v2) (listAddH xs ys)
      |StaticNum n <- x, StaticVec m <- y = Right $ Just $ ResolveException $ T.pack "Vectors with non-matching element types passed to addition."
      |StaticVec v1 <- x, StaticNum v2 <- y = Right $ Just $ ResolveException $ T.pack "Vectors with non-matching element types passed to addition."
      |otherwise = Right Nothing
    finisher :: Either [OpStatic] (Maybe ResolveException) -> FunRes
    finisher (Left list) = Resolved $ TypeStatic $ StaticVec $ OpVector list
    finisher (Right Nothing) = NeedsRolls
    finisher (Right (Just e)) = Errored e
    listAdd :: [OpStatic] -> [OpStatic] -> FunRes
    listAdd list1 list2
      |length list1 == length list2 = finisher $ listAddH list1 list2
      |otherwise = Errored $ ResolveException $ T.pack "Vectors of different lengths passed to addition."

opAdd :: OpType -> OpType -> FunRes
opAdd (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed as left argument to addition."
opAdd a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed as right argument to addition."
opAdd (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Resolved $ TypeStatic $ StaticNum $ x + y
opAdd (TypeStatic (StaticVec x)) (TypeStatic (StaticVec y)) = vecAdd x y
opAdd (TypeStatic (StaticNum x)) (TypeStatic (StaticVec y)) = Errored $ ResolveException $ T.pack "Number passed as left argument and vector passed as right argument to addition."
opAdd (TypeStatic (StaticVec x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Vector passed as left argument and number passed as right argument to addition."
opAdd a b = NeedsRolls

opOr :: OpType -> OpType -> FunRes
opOr (TypeStatic (StaticDie x)) b = NeedsRolls
opOr a (TypeStatic (StaticDie y)) = NeedsRolls
opOr (TypeStatic x) (TypeStatic y) = Resolved $ TypeStatic $ StaticBool $ toBool x || toBool y
opOr a b = NeedsRolls

opNeg :: OpType -> FunRes
opNeg (TypeStatic (StaticBool b)) = Errored $ ResolveException $ T.pack "Boolean passed as argument to negation."
opNeg (TypeStatic (StaticNum x)) = Resolved $ TypeStatic $ StaticNum $ negate x
opNeg (TypeStatic (StaticVec v)) =  smartVecMult (GReal (GSimp (GInt (-1)))) v
opNeg a = NeedsRolls

vecMinus :: OpVector -> OpVector -> FunRes
vecMinus (OpVector list1) (OpVector list2) = listMinus list1 list2
  where
    smartCons :: Either OpStatic (Either [OpStatic] (Maybe ResolveException)) -> Either [OpStatic] (Maybe ResolveException) -> Either [OpStatic] (Maybe ResolveException)
    smartCons (Left x) rest
      |Left list <- rest = Left (x:list)
      |Right Nothing <- rest = Right Nothing
      |Right (Just e) <- rest = Right $ Just e
    smartCons (Right x) rest
      |Left list <- x = smartCons (Left $ StaticVec $ OpVector list) rest
      |Right Nothing <- x = Right Nothing
      |Right (Just e) <- x = Right $ Just e
    listMinusH :: [OpStatic] -> [OpStatic] -> Either [OpStatic] (Maybe ResolveException)
    listMinusH [] [] = Left []
    listMinusH (x:xs) (y:ys)
      |StaticBool b <- x = Right $ Just $ ResolveException $ T.pack "Vector containing bool passed to subtraction."
      |StaticBool b <- y = Right $ Just $ ResolveException $ T.pack "Vector containing bool passed to subtraction."
      |StaticNum n <- x, StaticNum m <- y = smartCons (Left $ StaticNum $ n-m) (listMinusH xs ys)
      |StaticVec (OpVector v1) <- x, StaticVec (OpVector v2) <- y = smartCons (Right $ listMinusH v1 v2) (listMinusH xs ys)
      |StaticNum n <- x, StaticVec m <- y = Right $ Just $ ResolveException $ T.pack "Vectors with non-matching element types passed to subtraction."
      |StaticVec v1 <- x, StaticNum v2 <- y = Right $ Just $ ResolveException $ T.pack "Vectors with non-matching element types passed to subtraction."
      |otherwise = Right Nothing
    finisher :: Either [OpStatic] (Maybe ResolveException) -> FunRes
    finisher (Left list) = Resolved $ TypeStatic $ StaticVec $ OpVector list
    finisher (Right Nothing) = NeedsRolls
    finisher (Right (Just e)) = Errored e
    listMinus :: [OpStatic] -> [OpStatic] -> FunRes
    listMinus list1 list2
      |length list1 == length list2 = finisher $ listMinusH list1 list2
      |otherwise = Errored $ ResolveException $ T.pack "Vectors of different lengths passed to subtraction."

opMinus :: OpType -> OpType -> FunRes
opMinus (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed as left argument to subtraction."
opMinus a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed as right argument to subtraction."
opMinus (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Resolved $ TypeStatic $ StaticNum $ x - y
opMinus (TypeStatic (StaticVec x)) (TypeStatic (StaticVec y)) = vecMinus x y
opMinus (TypeStatic (StaticNum x)) (TypeStatic (StaticVec y)) = Errored $ ResolveException $ T.pack "Number passed as left argument and vector passed as right argument to subtraction."
opMinus (TypeStatic (StaticVec x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Vector passed as left argument and number passed as right argument to subtraction."
opMinus a b = NeedsRolls

vecComp :: OpType -> OpVector -> (GeneralRealNumber -> GeneralRealNumber -> Bool) -> FunRes
vecComp (TypeStatic (StaticNum (GReal n))) (OpVector list) fun
  = vecComp (TypeStatic $ StaticVec $ OpVector $ replicate (length list) (StaticNum $ GReal n)) (OpVector list) fun
vecComp (TypeStatic (StaticNum x)) v fun = Errored $ ResolveException $ T.pack "Unordered number passed to comparison."
vecComp (TypeStatic (StaticVec (OpVector x))) (OpVector y) fun = listComp x y fun
  where
    listCompH :: [OpStatic] -> [OpStatic] -> (GeneralRealNumber -> GeneralRealNumber -> Bool) -> Either Bool (Maybe ResolveException)
    listCompH [] [] fun = Left True
    listCompH (StaticNum (GComp n):xs) b fun = Right $ Just $ ResolveException $ T.pack "Vector with non-ordered number passed to comparison."
    listCompH a (StaticNum (GComp n):ys) fun = Right $ Just $ ResolveException $ T.pack "Vector with non-ordered number passed to comparison."
    listCompH (StaticBool a:xs) b fun = Right $ Just $ ResolveException $ T.pack "Vector with boolean passed to comparison."
    listCompH a (StaticBool b:ys) fun = Right $ Just $ ResolveException $ T.pack "Vector with boolean passed to comparison."
    listCompH (StaticNum (GReal n):xs) (StaticNum (GReal m):ys) fun
      |fun n m = listCompH xs ys fun
      |otherwise = Left False
    listCompH (StaticVec (OpVector list1):xs) (StaticVec (OpVector list2):ys) fun
      |Left True <- res = listCompH xs ys fun
      |otherwise = res
      where
        res = listCompH list1 list2 fun
    listCompH (StaticNum n:xs) (StaticVec (OpVector list):ys) fun
      |Left True <- res = listCompH xs ys fun
      |otherwise = res
      where
        res = listCompH (replicate (length list) (StaticNum n)) list fun
    listCompH (StaticVec (OpVector list):ys) (StaticNum n:xs) fun
      |Left True <- res = listCompH xs ys fun
      |otherwise = res
      where
        fun' :: GeneralRealNumber -> GeneralRealNumber -> Bool
        fun' a b = fun b a
        res = listCompH (replicate (length list) (StaticNum n)) list fun'
    listCompH a b fun = Right Nothing
    finisher :: Either Bool (Maybe ResolveException) -> FunRes
    finisher (Left x) = Resolved $ TypeStatic $ StaticBool x
    finisher (Right Nothing) = NeedsRolls
    finisher (Right (Just e)) = Errored e
    listComp :: [OpStatic] -> [OpStatic] -> (GeneralRealNumber -> GeneralRealNumber -> Bool) -> FunRes
    listComp list1 list2 fun
      |length list1 == length list2 = finisher $ listCompH list1 list2 fun
      |otherwise = Errored $ ResolveException $ T.pack "Vectors of different lengths passed to comparison."
vecComp x y fun= NeedsRolls

successFun :: NumTest -> Dice -> OpType -> FunRes
successFun numTest d op2
  |TypeStatic x <- op2 = resParse d x
  |otherwise = Errored $ ResolveException $ T.pack "Non-number passed as right argument to reroll function."
  where
    resParse :: Dice -> OpStatic -> FunRes
    resParse d x
      |Left test <- res = Resolved $ TypeStatic $ StaticDie $ addSuccess d test
      |Right (Left _) <- res = NeedsRolls
      |Right (Right e) <- res = Errored e
      where
        res = parseNumTestToken numTest x

opGre :: OpType -> OpType -> FunRes
opGre (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed to greater than."
opGre a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed to greater than."
opGre (TypeStatic (StaticNum (GReal n))) (TypeStatic (StaticNum (GReal m))) = Resolved $ TypeStatic $ StaticBool $ n > m
opGre (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Unordered number passed to greater than."
opGre a (TypeStatic (StaticVec y)) = vecComp a y (>)
opGre (TypeStatic (StaticDie d)) op2 = successFun (TestGre 0) d op2
opGre a b = NeedsRolls

opLess :: OpType -> OpType -> FunRes
opLess (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed to less than."
opLess a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed to less than."
opLess (TypeStatic (StaticNum (GReal n))) (TypeStatic (StaticNum (GReal m))) = Resolved $ TypeStatic $ StaticBool $ n < m
opLess (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Unordered number passed to less than."
opLess a (TypeStatic (StaticVec y)) = vecComp a y (<)
opLess (TypeStatic (StaticDie d)) op2 = successFun (TestLes 0) d op2
opLess a b = NeedsRolls

opGeq :: OpType -> OpType -> FunRes
opGeq (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed to greater than or equal to."
opGeq a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed to greater than or equal to."
opGeq (TypeStatic (StaticNum (GReal n))) (TypeStatic (StaticNum (GReal m))) = Resolved $ TypeStatic $ StaticBool $ n >= m
opGeq (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Unordered number passed to greater than."
opGeq a (TypeStatic (StaticVec y)) = vecComp a y (>=)
opGeq (TypeStatic (StaticDie d)) op2 = successFun (TestGeq 0) d op2
opGeq a b = NeedsRolls

opLeq :: OpType -> OpType -> FunRes
opLeq (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed to less than or equal to."
opLeq a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed to less than or equal to."
opLeq (TypeStatic (StaticNum (GReal n))) (TypeStatic (StaticNum (GReal m))) = Resolved $ TypeStatic $ StaticBool $ n <= m
opLeq (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Unordered number passed to less than or equal to."
opLeq a (TypeStatic (StaticVec y)) = vecComp a y (<=)
opLeq (TypeStatic (StaticDie d)) op2 = successFun (TestLeq 0) d op2
opLeq a b = NeedsRolls

opEq :: OpType -> OpType -> FunRes
opEq (TypeStatic (StaticDie d)) op2 = successFun (TestEq []) d op2
opEq (TypeStatic x) (TypeStatic y) = Resolved $ TypeStatic $ StaticBool $ x == y
opEq x y = NeedsRolls

opNeq :: OpType -> OpType -> FunRes
opNeq (TypeStatic (StaticDie d)) op2 = successFun (TestNeq []) d op2
opNeq (TypeStatic x) (TypeStatic y) = Resolved $ TypeStatic $ StaticBool $ x /= y
opNeq x y = NeedsRolls

opInRange :: OpType -> OpType -> FunRes
opInRange (TypeStatic (StaticDie d)) op2 = successFun (TestIn 0 0) d op2
opInRange (TypeStatic _) _ = Errored $ ResolveException $ T.pack "Non-die passed as left argument to In."
opInRange _ _ = NeedsRolls

opOutRange :: OpType -> OpType -> FunRes
opOutRange (TypeStatic (StaticDie d)) op2 = successFun (TestOut 0 0) d op2
opOutRange (TypeStatic _) _ = Errored $ ResolveException $ T.pack "Non-die passed as left argument to Out."
opOutRange _ _ = NeedsRolls

{-opIf :: [OpType] -> FunRes
opIf [] = Errored $ ResolveException $ T.pack "If called with no arguments."
opIf [x] = Errored $ ResolveException $ T.pack "If called with one argument."
opIf [x,y]
  |TypeStatic (StaticBool False) <- x = Resolved x
  |TypeStatic (StaticNum 0) <- x = Resolved $ TypeStatic $ StaticBool False
  |TypeStatic (StaticVec (OpVector [])) <- x = Resolved $ TypeStatic $ StaticBool False
  |TypeStatic (StaticDie d) <- x = NeedsRolls
  |TypeStatic a <- x = Resolved y
  |otherwise = NeedsRolls
opIf (x:y:z:rest)
  |TypeStatic (StaticBool False) <- x = Resolved z
  |TypeStatic (StaticNum 0) <- x = Resolved z
  |TypeStatic (StaticVec (OpVector [])) <- x = Resolved z
  |TypeStatic (StaticDie d) <- x = NeedsRolls
  |TypeStatic a <- x = Resolved y
  |otherwise = NeedsRolls-}

opIf :: [OpType] -> FunRes
opIf [] = Errored $ ResolveException $ T.pack "If called with no arguments."
opIf [TypeStatic (StaticVec (OpVector [x,y]))]
  |StaticBool False <- x = Resolved $ TypeStatic x
  |StaticNum 0 <- x = Resolved $ TypeStatic $ StaticBool False
  |StaticVec (OpVector []) <- x = Resolved $ TypeStatic $ StaticBool False
  |StaticDie d <- x = NeedsRolls
  |otherwise = Resolved $ TypeStatic y
opIf [TypeStatic (StaticVec (OpVector (x:y:z:rest)))]
  |StaticBool False <- x = Resolved $ TypeStatic z
  |StaticNum 0 <- x = Resolved $ TypeStatic z
  |StaticVec (OpVector []) <- x = Resolved $ TypeStatic z
  |StaticDie d <- x = NeedsRolls
  |otherwise = Resolved $ TypeStatic y
opIf [TypeStatic x] = Errored $ ResolveException $ T.pack "If called with one argument."
opIf [_] = NeedsRolls

opCeil :: [OpType] -> FunRes
opCeil [] = Errored $ ResolveException $ T.pack "Ceil called with no arguments."
opCeil (x:rest)
  |TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- x = Resolved x
  |TypeStatic (StaticNum (GReal n)) <- x = Resolved $ TypeStatic $ StaticNum $ GReal $ GSimp $ GInt $ ceiling $ genRealtoFloat n
  |TypeStatic (StaticNum y) <- x = Errored $ ResolveException $ T.pack "Ceil called on a complex number."
  |TypeStatic (StaticDie d) <- x = NeedsRolls
  |TypeStatic y <- x = Errored $ ResolveException $ T.pack "Ceil called on non-number."
  |otherwise = NeedsRolls

opRound :: [OpType] -> FunRes
opRound [] = Errored $ ResolveException $ T.pack "Round called with no arguments."
opRound (x:rest)
  |TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- x = Resolved x
  |TypeStatic (StaticNum (GReal n)) <- x = Resolved $ TypeStatic $ StaticNum $ GReal $ GSimp $ GInt $ round $ genRealtoFloat n
  |TypeStatic (StaticNum y) <- x = Errored $ ResolveException $ T.pack "Round called on a complex number."
  |TypeStatic (StaticDie d) <- x = NeedsRolls
  |TypeStatic y <- x = Errored $ ResolveException $ T.pack "Round called on non-number."
  |otherwise = NeedsRolls

opFloor :: [OpType] -> FunRes
opFloor [] = Errored $ ResolveException $ T.pack "Floor called with no arguments."
opFloor (x:rest)
  |TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- x = Resolved x
  |TypeStatic (StaticNum (GReal n)) <- x = Resolved $ TypeStatic $ StaticNum $ GReal $ GSimp $ GInt $ floor $ genRealtoFloat n
  |TypeStatic (StaticNum y) <- x = Errored $ ResolveException $ T.pack "Floor called on a complex number."
  |TypeStatic (StaticDie d) <- x = NeedsRolls
  |TypeStatic y <- x = Errored $ ResolveException $ T.pack "Floor called on non-number."
  |otherwise = NeedsRolls

vecMax :: [OpStatic] -> (GeneralRealNumber -> GeneralRealNumber -> Bool) -> FunRes
vecMax list fun = listMax Nothing list
  where
    listMax :: Maybe GeneralRealNumber -> [OpStatic] -> FunRes
    listMax (Just m) [] = Resolved $ TypeStatic $ StaticNum $ GReal m
    listMax Nothing (StaticNum (GReal n):rest) = listMax (Just n) rest
    listMax (Just m) (StaticNum (GReal n):rest)
      |fun n m = listMax (Just n) rest
      |otherwise = listMax (Just m) rest
    listMax m (StaticNum x:rest) = Errored $ ResolveException $ T.pack "Complex number passed to comparison."
    listMax m n = Errored $ ResolveException $ T.pack "Non-number passed to comparison."

opMax :: [OpType] -> FunRes
opMax [] = Errored $ ResolveException $ T.pack "Max called with no arguments."
opMax (x:rest)
  |TypeStatic (StaticVec (OpVector list)) <- x = vecMax list (>)
  |TypeStatic (StaticNum n) <- x = Resolved x
  |TypeStatic (StaticBool b) <- x = Errored $ ResolveException $ T.pack "Max called on non-number."
  |otherwise = NeedsRolls

opMin :: [OpType] -> FunRes
opMin [] = Errored $ ResolveException $ T.pack "Min called with no arguments."
opMin (x:rest)
  |TypeStatic (StaticVec (OpVector list)) <- x = vecMax list (<)
  |TypeStatic (StaticNum n) <- x = Resolved x
  |TypeStatic (StaticBool b) <- x = Errored $ ResolveException $ T.pack "Min called on non-number."
  |otherwise = NeedsRolls

vecMerge :: GeneralNumber -> [OpStatic] -> (GeneralNumber -> GeneralNumber -> GeneralNumber) -> FunRes
vecMerge identity list fun = finisher $ listMerge list
  where
    smartFun :: GeneralNumber -> Either GeneralNumber (Maybe ResolveException) -> Either GeneralNumber (Maybe ResolveException)
    smartFun n (Left m) = Left $ fun n m
    smartFun n (Right e) = Right e
    listMerge :: [OpStatic] -> Either GeneralNumber (Maybe ResolveException)
    listMerge [] = Left identity
    listMerge (x:xs)
      |StaticBool b <- x = Right $ Just $ ResolveException $ T.pack "Non-numeric argument passed to number-based function."
      |StaticVec v <- x = Right $ Just $ ResolveException $ T.pack "Non-numeric argument passed to number-based function."
      |StaticNum n <- x = smartFun n (listMerge xs)
      |otherwise = Right Nothing
    finisher :: Either GeneralNumber (Maybe ResolveException) -> FunRes
    finisher (Left n) = Resolved $ TypeStatic $ StaticNum n
    finisher (Right Nothing) = NeedsRolls
    finisher (Right (Just e)) = Errored e

opSum :: [OpType] -> FunRes
opSum [] = Resolved $ TypeStatic $ StaticNum 0
opSum (x:rest)
  |TypeStatic (StaticNum n) <- x = Resolved x
  |TypeStatic (StaticVec (OpVector v)) <- x = vecMerge 0 v (+)
  |TypeStatic (StaticBool b) <- x = Errored $ ResolveException $ T.pack "Non-numeric argument passed to sum."
  |otherwise = NeedsRolls

opProd :: [OpType] -> FunRes
opProd [] = Resolved $ TypeStatic $ StaticNum 1
opProd (x:rest)
  |TypeStatic (StaticNum n) <- x = Resolved x
  |TypeStatic (StaticVec (OpVector v)) <- x = vecMerge 1 v (*)
  |TypeStatic (StaticBool b) <- x = Errored $ ResolveException $ T.pack "Non-numeric argument passed to sum."
  |otherwise = NeedsRolls

--data OpVector = OpVector [OpStatic]

opVector :: [OpType] -> FunRes
opVector [] = Resolved $ TypeStatic $ StaticVec $ OpVector []
opVector list
  |Just statics <- childStaticTest list = Resolved $ TypeStatic $ StaticVec $ OpVector statics
  |otherwise = NeedsRolls

resTest :: [OpType] -> Maybe [OpStatic]
resTest [] = Just []
resTest (x:xs)
  |TypeStatic (StaticNum num) <- x = maybeCons (StaticNum num) $ resTest xs
  |TypeStatic (StaticVec (OpVector vec)) <- x, Just v <- vecResTest vec = maybeCons (StaticVec $ OpVector v) $ resTest xs
  |otherwise = Nothing
  where
    vecResTest :: [OpStatic] -> Maybe [OpStatic]
    vecResTest [] = Just []
    vecResTest (x:xs)
      |StaticNum num <- x = maybeCons (StaticNum num) $ vecResTest xs
      |StaticVec (OpVector vec) <- x, Just v <- vecResTest vec = maybeCons (StaticVec $ OpVector v) $ vecResTest xs
      |otherwise = Nothing

opResVector :: [OpType] -> FunRes
opResVector [] = Resolved $ TypeStatic $ StaticVec $ OpVector []
opResVector list
  |Just statics <- resTest list, [x] <- statics = Resolved $ TypeStatic x
  |Just statics <- resTest list = Resolved $ TypeStatic $ StaticVec $ OpVector statics
  |otherwise = NeedsRolls

opBool :: [OpType] -> FunRes
opBool [TypeStatic (StaticDie _)] = NeedsRolls
opBool [TypeStatic x] = Resolved $ TypeStatic $ StaticBool $ toBool x
opBool _ = NeedsRolls

data Assoc = AssocLeft | AssocRight | AssocNone deriving (Eq, Show)
--Argument to In/Pre/Post is prec, argument to Fun is number of args
data OpTokenType = OpTokenIn Integer | OpTokenPre Integer | OpTokenPost Integer | OpTokenFun Integer | OpTokenInOrPre Integer Integer | OpTokenInOrPost Integer Integer
  deriving (Show)
data OpToken = OpToken {optype :: OpTokenType, resolveOrder :: OpResolveOrder, assoc :: Assoc, function :: FunType}

instance Show OpToken where
  show OpToken {optype = ot, assoc = a} = "OpToken: (" ++ show ot ++ "," ++ show a ++ ")"

safeComp :: (GeneralRealNumber -> GeneralRealNumber -> Bool) -> GeneralNumber -> GeneralNumber -> Bool
safeComp fun (GReal n) (GReal m) = fun n m
safeComp fun n m = False

getType :: OpToken -> OpTokenType
getType OpToken {optype=t}=t

--rerollFun :: NumTest -> OpType -> OpType -> FunRes
--explodingFun :: NumTest -> OpType -> OpType -> FunRes

operatorDict = Map.fromList
  [(T.pack "d",     OpToken {optype=OpTokenInOrPre 12 12,   resolveOrder = OpResolveAll,        assoc = AssocLeft,  function = FPossUn (feedThrough dFunction) 12 (dFunction "" $ TypeStatic $ StaticNum 1) 12 Prefix                            }),
   (T.pack "#",     OpToken {optype=OpTokenIn 11,           resolveOrder = OpResolveLeftFirst,  assoc = AssocRight, function = FInfix  dupFunction 11                                             }),
   (T.pack "dF",    OpToken {optype=OpTokenPost 11,         resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FUnary  fudgeDie 11 Suffix                                         }),
   --(T.pack "k",   OpToken {optype=OpTokenIn 10,         resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (keepdropFun KeepHigh) 10}),
   (T.pack "kh",    OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (keepdropFun KeepHigh) 10                                  }),
   (T.pack "kl",    OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (keepdropFun KeepLow) 10                                   }),
   (T.pack "dh",    OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (keepdropFun DropHigh) 10                                  }),
   (T.pack "dl",    OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (keepdropFun DropLow) 10                                   }),
   (T.pack "r",     OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (rerollFun (TestEq [0])) 10                                }),
   (T.pack "r<",    OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (rerollFun (TestLes 0)) 10                                 }),
   (T.pack "r<=",   OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (rerollFun (TestLeq 0)) 10                                 }),
   (T.pack "r>",    OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (rerollFun (TestGre 0)) 10                                 }),
   (T.pack "r>=",   OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (rerollFun (TestGeq 0)) 10                                 }),
   (T.pack "rIn",   OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (rerollFun (TestIn 0 0)) 10                                }),
   (T.pack "rOut",  OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (rerollFun (TestOut 0 0)) 10                               }),
   (T.pack "!",     OpToken {optype=OpTokenInOrPost 10 10,  resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FPossUn (explodingFun (TestEq [0])) 8 exclamationpoint 8 Suffix    }),
   (T.pack "!<",    OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (explodingFun (TestLes 0)) 10                              }),
   (T.pack "!<=",   OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (explodingFun (TestLeq 0)) 10                              }),
   (T.pack "!>",    OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (explodingFun (TestGre 0)) 10                              }),
   (T.pack "!>=",   OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (explodingFun (TestGeq 0)) 10                              }),
   (T.pack "!In",   OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (explodingFun (TestIn 0 0)) 10                             }),
   (T.pack "!Out",  OpToken {optype=OpTokenIn 10,           resolveOrder = OpResolveRight,      assoc = AssocNone,  function = FInfix  (explodingFun (TestOut 0 0)) 10                            }),
   (T.pack "~",     OpToken {optype=OpTokenPre 6,           resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FUnary  opNot 6 Prefix                                             }),
   (T.pack "not",   OpToken {optype=OpTokenFun 1,           resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FFunct  opNotFun                                                   }),
   (T.pack "^",     OpToken {optype=OpTokenIn 4,            resolveOrder = OpResolveAll,        assoc = AssocRight, function = FInfix  opExp 4                                                    }),
   (T.pack "**",    OpToken {optype=OpTokenIn 4,            resolveOrder = OpResolveAll,        assoc = AssocRight, function = FInfix  opExp 4                                                    }),
   (T.pack "&&",    OpToken {optype=OpTokenIn 3,            resolveOrder = OpResolveAll,        assoc = AssocLeft,  function = FInfix  opAdd 2                                                    }),
   (T.pack "and",   OpToken {optype=OpTokenIn 3,            resolveOrder = OpResolveAll,        assoc = AssocLeft,  function = FInfix  opAdd 2                                                    }),
   (T.pack "*",     OpToken {optype=OpTokenIn 3,            resolveOrder = OpResolveAll,        assoc = AssocLeft,  function = FInfix  opMult 3                                                   }),
   (T.pack "/",     OpToken {optype=OpTokenIn 3,            resolveOrder = OpResolveAll,        assoc = AssocLeft,  function = FInfix  opDiv 3                                                    }),
   (T.pack "%",     OpToken {optype=OpTokenIn 3,            resolveOrder = OpResolveAll,        assoc = AssocLeft,  function = FInfix  opMod 3                                                    }),
   (T.pack "||",    OpToken {optype=OpTokenIn 2,            resolveOrder = OpResolveAll,        assoc = AssocLeft,  function = FInfix  opOr 2                                                     }),
   (T.pack "or",    OpToken {optype=OpTokenIn 2,            resolveOrder = OpResolveAll,        assoc = AssocLeft,  function = FInfix  opOr 2                                                     }),
   (T.pack "+",     OpToken {optype=OpTokenIn 2,            resolveOrder = OpResolveAll,        assoc = AssocLeft,  function = FInfix  opAdd 2                                                    }),
   (T.pack "-",     OpToken {optype=OpTokenInOrPre 2 6,     resolveOrder = OpResolveAll,        assoc = AssocLeft,  function = FPossUn opMinus 2 opNeg 6 Prefix                                  }),
   (T.pack ">",     OpToken {optype=OpTokenIn 1,            resolveOrder = OpResolveRightFirst, assoc = AssocLeft,  function = FInfix  opGre 1                                                    }),
   (T.pack "<",     OpToken {optype=OpTokenIn 1,            resolveOrder = OpResolveRightFirst, assoc = AssocLeft,  function = FInfix  opLess 1                                                   }),
   (T.pack ">=",    OpToken {optype=OpTokenIn 1,            resolveOrder = OpResolveRightFirst, assoc = AssocLeft,  function = FInfix  opGeq 1                                                    }),
   (T.pack "<=",    OpToken {optype=OpTokenIn 1,            resolveOrder = OpResolveRightFirst, assoc = AssocLeft,  function = FInfix  opLeq 1                                                    }),
   (T.pack "==",    OpToken {optype=OpTokenIn 1,            resolveOrder = OpResolveRightFirst, assoc = AssocLeft,  function = FInfix  opEq 1                                                     }),
   (T.pack "/=",    OpToken {optype=OpTokenIn 1,            resolveOrder = OpResolveRightFirst, assoc = AssocLeft,  function = FInfix  opNeq 1                                                    }),
   (T.pack "In",    OpToken {optype=OpTokenIn 1,            resolveOrder = OpResolveRightFirst, assoc = AssocLeft,  function = FInfix  opInRange 1                                                }),
   (T.pack "Out",   OpToken {optype=OpTokenIn 1,            resolveOrder = OpResolveRightFirst, assoc = AssocLeft,  function = FInfix  opOutRange 1                                               }),
   (T.pack "if",    OpToken {optype=OpTokenFun 3,           resolveOrder = OpResolveLeft,       assoc = AssocNone,  function = FFunct  opIf                                                       }),
   (T.pack "ceil",  OpToken {optype=OpTokenFun 1,           resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FFunct  opCeil                                                     }),
   (T.pack "round", OpToken {optype=OpTokenFun 1,           resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FFunct  opRound                                                    }),
   (T.pack "floor", OpToken {optype=OpTokenFun 1,           resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FFunct  opFloor                                                    }),
   (T.pack "max",   OpToken {optype=OpTokenFun 1,           resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FFunct  opMax                                                      }),
   (T.pack "min",   OpToken {optype=OpTokenFun 1,           resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FFunct  opMin                                                      }),
   (T.pack "sum",   OpToken {optype=OpTokenFun 1,           resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FFunct  opSum                                                      }),
   (T.pack "prod",  OpToken {optype=OpTokenFun 1,           resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FFunct  opProd                                                     }),
   (T.pack "bool",  OpToken {optype=OpTokenFun 1,           resolveOrder = OpResolveAll,        assoc = AssocNone,  function = FFunct  opBool                                                     })]

operatorKeys = lensort $ Map.keys operatorDict
  where
    leq :: T.Text -> T.Text -> Bool
    leq str1 str2
      |T.length str1 > T.length str2 = True
      |T.length str1 == T.length str2 && (str1 <= str2) = True
      |otherwise = False
    gre :: T.Text -> T.Text -> Bool
    gre str1 str2
      |T.length str1 < T.length str2 = True
      |T.length str1 == T.length str2 && (str1 > str2) = True
      |otherwise = False
    lensort :: [T.Text] -> [T.Text]
    lensort [] = []
    lensort (str:rest) = lensort greater ++ [str] ++ lensort lesser
      where
        lesser = filter (leq str) rest
        greater = filter (gre str) rest
