module HistoryDiceParser.Operators
(KeepDrop(..)
,IntTest(..)
,TestInput(..)
,smartIntegerToInt
,Dice(..)
,createDie
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

import Debug.Trace

import General.UserNumber

data KeepDrop = KeepHigh | DropHigh | KeepLow | DropLow
  deriving (Eq)

data IntTest = IntTest {testLes :: Maybe Int, testEq :: Maybe [Int], testGre :: Maybe Int}
  deriving (Show, Eq)
data TestInput = TestLeq Int | TestLes Int | TestGeq Int | TestGre Int | TestEq Int
  deriving (Show)

smartIntegerToInt :: Integer -> Int
smartIntegerToInt n
  |n > fromIntegral (maxBound::Int) = maxBound::Int
  |n < fromIntegral (minBound::Int) = minBound::Int
  |otherwise = fromInteger n

inputParse :: Either Int [GeneralNumber] -> (TestInput, Either GeneralNumber (GeneralNumber -> Bool)) -> Either IntTest [GeneralNumber] -> (Either IntTest [GeneralNumber], Bool)
inputParse (Left n) (input, _) (Left intTest@IntTest {testLes=les, testEq=eq, testGre=gre})
  |TestLeq m <- input, Nothing   <- les               = (Left $ intTest {testLes=Just $ m + 1}, True)
  |TestLeq m <- input, Just oldM <- les, m + 1> oldM  = (Left $ intTest {testLes=Just $ m + 1}, True)
  |TestLeq m <- input, Just oldM <- les               = (Left intTest, False)
  |TestLes m <- input, Nothing   <- les               = (Left $ intTest {testLes=Just m}, True)
  |TestLes m <- input, Just oldM <- les, m > oldM     = (Left $ intTest {testLes=Just m}, True)
  |TestLes m <- input, Just oldM <- les               = (Left intTest, False)
  |TestEq  m <- input, Nothing   <- eq                = (Left $ intTest {testEq = Just [m]}, True)
  |TestEq  m <- input, Just list <- eq, m `elem` list = (Left intTest, False)
  |TestEq  m <- input, Just list <- eq                = (Left $ intTest {testEq = Just (m:list)}, True)
  |TestGre m <- input, Nothing   <- gre               = (Left $ intTest {testGre=Just m}, True)
  |TestGre m <- input, Just oldM <- gre, m > oldM     = (Left $ intTest {testGre=Just m}, True)
  |TestGre m <- input, Just oldM <- gre               = (Left intTest, False)
  |TestGeq m <- input, Nothing   <- gre               = (Left $ intTest {testGre=Just $ m - 1}, True)
  |TestGeq m <- input, Just oldM <- gre, m - 1< oldM  = (Left $ intTest {testGre=Just $ m - 1}, True)
  |TestGeq m <- input, Just oldM <- gre               = (Left intTest, False)
inputParse (Right nums) (_, Left number) (Right list)
  |number `elem` list = (Right list, False)
  |otherwise = (Right (number:list), True)
inputParse (Right nums) (_, Right predicate) (Right list) = (Right $ union list newInfo, not(null newInfo))
  where
    newInfo = [n | n <- nums, n `notElem` list && predicate n]

data Dice = Die {poolSize::      Int,
                 poolDis::       String,
                 face::          Either Int [GeneralNumber],
                 faceDis::       String,
                 exploding::     Either IntTest [GeneralNumber],
                 reroll::        Either IntTest [GeneralNumber],
                 keep_drop::     Maybe Int,
                 keep_drop_type::Maybe KeepDrop,
                 success::       Either IntTest [GeneralNumber],
                 suffix::        String}
  deriving (Eq)

createDie :: Int -> String -> Either Int [GeneralNumber] -> String -> Dice
createDie poolnum poolstr facerep facestring =
          Die {poolSize=poolnum,
              poolDis=poolstr,
              face=facerep,
              faceDis=facestring,
              exploding=emptytest,
              reroll=emptytest,
              keep_drop=Nothing,
              keep_drop_type=Nothing,
              success=emptytest,
              suffix=""}
  where
    makeempty :: Either Int [GeneralNumber] -> Either IntTest [GeneralNumber]
    makeempty (Left n) = Left IntTest {testLes=Nothing, testGre=Nothing, testEq=Nothing}
    makeempty (Right n) = Right []
    emptytest = makeempty facerep

addExplode :: Dice -> (TestInput, Either GeneralNumber (GeneralNumber -> Bool)) -> String -> Dice
addExplode die@Die {face=faceposs, exploding=explodlist, suffix=suff} testTup string = die {exploding = res, suffix = suff ++ (if test then string else "")}
  where
    (res, test) = inputParse faceposs testTup explodlist

addReroll :: Dice -> (TestInput, Either GeneralNumber (GeneralNumber -> Bool)) -> String -> Dice
addReroll die@Die {face=faceposs, reroll=rerolllist, suffix=suff} testTup string = die {reroll = res, suffix = suff ++ (if test then string else "")}
  where
    (res, test) = inputParse faceposs testTup rerolllist

addKeepDrop :: Dice -> KeepDrop -> Int -> Dice
addKeepDrop die@Die {suffix=suff} kdt num = die {keep_drop=Just num, keep_drop_type=Just kdt, suffix = suff ++ dis kdt ++ show num}
  where
    dis :: KeepDrop -> String
    dis KeepHigh = "k"
    dis KeepLow = "kl"
    dis DropHigh = "dh"
    dis DropLow = "d"

addSuccess :: Dice -> (TestInput, Either GeneralNumber (GeneralNumber -> Bool)) -> String -> Dice
addSuccess die@Die {face=faceposs, success=successlist, suffix=suff} testTup string = die {success = res, suffix = suff ++ (if test then string else "")}
  where
    (res, test) = inputParse faceposs testTup successlist

instance Show Dice where
  show Die {poolDis=pool, faceDis=fac, suffix=suff} = pool ++ "d" ++ fac ++ suff

newtype OpVector = OpVector [OpStatic]
  deriving (Eq)

vecMap f (OpVector list) = OpVector $ map f list

instance Show OpVector where
  show (OpVector list) = "(" ++ intercalate ", " (map show list) ++ ")"

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

vecIntTest :: OpVector -> Maybe [Integer]
vecIntTest (OpVector list) = res
  where
    vecIntTestH :: [OpStatic] -> Maybe [Integer]
    vecIntTestH [] = Just []
    vecIntTestH (x:xs)
      |StaticNum (GReal (GSimp (GInt n))) <- x = maybeCons n (vecIntTestH xs)
      |otherwise = Nothing
    res = vecIntTestH list

vecPosIntTest :: OpVector -> Maybe [Integer]
vecPosIntTest (OpVector list) = res
  where
    vecIntTestH :: [OpStatic] -> Maybe [Integer]
    vecIntTestH [] = Just []
    vecIntTestH (x:xs)
      |StaticNum (GReal (GSimp (GInt n))) <- x, n >= 0 = maybeCons n (vecIntTestH xs)
      |otherwise = Nothing
    res = vecIntTestH list

vecNumTest :: OpVector -> Maybe [GeneralNumber]
vecNumTest (OpVector list) = res
  where
    vecNumTestH :: [OpStatic] -> Maybe [GeneralNumber]
    vecNumTestH [] = Just []
    vecNumTestH (x:xs)
      |StaticNum num <- x = maybeCons num (vecNumTestH xs)
      |otherwise = Nothing
    res = vecNumTestH list

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

dFunction :: OpType -> OpType -> FunRes
dFunction (TypeStatic (StaticNum num)) op2
  |GReal (GSimp (GInt m)) <- num, m>0, TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- op2, n > 0
    = moldres $ Left $ createDie (smartIntegerToInt m) (show $ smartIntegerToInt m) (Left $ smartIntegerToInt n) (show $ smartIntegerToInt n)
  |GReal (GSimp (GInt m)) <- num, m>0, TypeStatic (StaticNum n) <- op2 = Errored $ ResolveException $ T.pack "Non-positive number passed as right argument to d."
  |GReal (GSimp (GInt m)) <- num, m>0, TypeStatic (StaticVec (OpVector [])) <- op2 = Errored $ ResolveException $ T.pack "Empty list passed as right argument to d."
  |GReal (GSimp (GInt m)) <- num, m>0, TypeStatic (StaticVec vec) <- op2, Just v <- vecNumTest vec
    = moldres $ Left $ createDie (smartIntegerToInt m) (show $ smartIntegerToInt m) (Right v) (show vec)
  |GReal (GSimp (GInt m)) <- num, m>0 = NeedsRolls
  |GReal (GSimp (GInt m)) <- num = Errored $ ResolveException $ T.pack "Non-positive number passed as left argument to d."
  |otherwise = Errored $ ResolveException $ T.pack "Non-integer number passed as left argument to d."
  where
    moldres :: Either Dice ResolveException -> FunRes
    moldres (Left x) = Resolved $ TypeStatic $ StaticDie x
    moldres (Right y) = Errored y
dFunction (TypeStatic (StaticDie d)) op2
  |TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- op2
    = Resolved $ TypeStatic $ StaticDie $ addKeepDrop d DropLow $ smartIntegerToInt n
  |TypeStatic (StaticNum n) <- op2 = Errored $ ResolveException $ T.pack "Die passed as left argument, invalid (i.e. non-integer) number as right argument to d."
  |TypeStatic (StaticVec v) <- op2 = Errored $ ResolveException $ T.pack "Die passed as left argument, vector passed as right argument to d."
dFunction (TypeStatic (StaticVec vec)) op2 = Errored $ ResolveException $ T.pack "Vector passed as left argument to d."
dFunction (TypeStatic (StaticBool b)) op2 = Errored $ ResolveException $ T.pack "Boolean passed as left argument to d."
dFunction op1 op2 = NeedsRolls

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
    fudgelist = [GReal (GSimp (GInt (-1))), GReal (GSimp (GInt 0)), GReal (GSimp (GInt 1))]
    moldres :: Either Dice ResolveException -> FunRes
    moldres (Left x) = Resolved $ TypeStatic $ StaticDie x
    moldres (Right y) = Errored y
fudgeDie (TypeStatic (StaticVec vec)) = Errored $ ResolveException $ T.pack "Vector passed as argument to dF."
fudgeDie (TypeStatic (StaticBool b)) = Errored $ ResolveException $ T.pack "Boolean passed as argument to dF."
fudgeDie op = NeedsRolls

keepdropFun :: KeepDrop -> OpType -> OpType -> FunRes
keepdropFun kd op1 op2
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- op2, n >=0 = Resolved $ TypeStatic $ StaticDie $ addKeepDrop d kd $ smartIntegerToInt n
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticNum (GReal (GSimp (GInt n)))) <- op2 = Errored $ ResolveException $ T.pack "Negative number passed as right argument to keep/drop function."
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticNum x) <- op2 = Errored $ ResolveException $ T.pack "Non-integer number passed as right argument to keep/drop function."
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticVec v) <- op2 = Errored $ ResolveException $ T.pack "Non-number passed as right argument to keep/drop function."
  |TypeStatic (StaticDie d) <- op1 = NeedsRolls
  |TypeStatic x <- op1 = Errored $ ResolveException $ T.pack "Non-die passed as left argument to keep/drop function."
  |otherwise = NeedsRolls

rerollFun :: (Int -> TestInput, Maybe (GeneralNumber -> GeneralNumber -> Bool), String) -> OpType -> OpType -> FunRes
rerollFun (testInput, testPred, string) op1 op2
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticNum (GReal x)) <- op2 = Resolved $ TypeStatic $ StaticDie $ addReroll d (testInput $ floor x, parseTest testPred $ GReal x) string
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticVec (OpVector list)) <- op2, Nothing <- testPred = rerollOneOfList d list
  |TypeStatic (StaticDie d) <- op1, TypeStatic (StaticVec v) <- op2 = Errored $ ResolveException $ T.pack "Vector passed as right argument to ordered reroll function."
  |TypeStatic (StaticDie d) <- op1 = Errored $ ResolveException $ T.pack "Non-number passed as right argument to reroll function."
  |TypeStatic x <- op1 = Errored $ ResolveException $ T.pack "Non-die passed as left argument to reroll function."
  |otherwise = NeedsRolls
  where
    parseTest :: Maybe (GeneralNumber -> GeneralNumber -> Bool) -> GeneralNumber -> Either GeneralNumber (GeneralNumber -> Bool)
    parseTest Nothing num = Left num
    parseTest (Just fun) num = Right $ fun num
    makesure :: [OpStatic] -> FunRes
    makesure [] = NeedsRolls
    makesure (StaticVec v:xs) = Errored $ ResolveException $ T.pack "Nested vector passed as right argument to reroll function"
    makesure (x:xs) = makesure xs
    smartTest :: GeneralNumber -> TestInput
    smartTest (GReal (GSimp (GInt n))) = TestEq $ smartIntegerToInt n
    smartTest n = TestEq 0
    rerollOneOfList :: Dice -> [OpStatic] -> FunRes
    rerollOneOfList d [] = Resolved $ TypeStatic $ StaticDie d
    rerollOneOfList d (x:xs)
      |StaticVec v <- x = Errored $ ResolveException $ T.pack "Nested vector passed as right argument to reroll function"
      |StaticVec v <- x = Errored $ ResolveException $ T.pack "Vector containing boolean(s) passed as right argument to reroll function"
      |StaticDie d <- x = makesure xs
      |StaticNum n <- x = rerollOneOfList (addReroll d (smartTest n, parseTest testPred n) string) xs

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

exclamationpoint :: OpType -> FunRes
exclamationpoint (TypeStatic (StaticDie d)) = maxExplode d
  where
    maxExplode :: Dice -> FunRes
    maxExplode d
      |Just (Left faceint) <- res = Resolved $ TypeStatic $ StaticDie $ addExplode d (TestEq faceint, Left $ GReal $ GSimp $ GInt $ fromIntegral faceint) "!"
      |Just (Right facelist) <- res = Resolved $ TypeStatic $ StaticDie $ addExplode d (TestEq 0, Left $ GReal $ maximum facelist) "!"
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

--Maybe (Either Integer [GeneralRealNumber])

explodingFun :: (Int -> TestInput) -> (GeneralNumber -> GeneralNumber -> Bool) -> String -> OpType -> OpType -> FunRes
explodingFun testInput fun string (TypeStatic (StaticDie d)) (TypeStatic (StaticNum x))
  |Just (Left faceint) <- res, GReal n <- x = traceShow n $ Resolved $ TypeStatic $ StaticDie $ addExplode d (testInput $ floor n, Left $ GReal $ GSimp $ GInt $ fromIntegral faceint) (string ++ show x)
  |Just (Right facelist) <- res, GReal n <- x = Resolved $ TypeStatic $ StaticDie $ addExplode d (TestEq 0, Right $ fun x) (string ++ show x)
  |Just facelist <- res = Errored $ ResolveException $ T.pack "Complex number passed on right side of ordered exploding function."
  |otherwise = Errored $ ResolveException $ T.pack "Die with non-ordered faces passed to left of exploding function."
  where
    res = realDieTest d
explodingFun _ _ _ (TypeStatic x) op2 = Errored $ ResolveException $ T.pack "Non-die passed to left of exploding function."
explodingFun _ _ _ _ _ = NeedsRolls

--data OpVector = OpVector [OpStatic]
--data OpStatic = StaticDie Dice | StaticNum GeneralNumber | StaticVec OpVector
--data OpType = TypeStatic OpStatic | TypeNode OpNode
--data FunType = FUnary (OpType -> FunRes) | FInfix (OpType -> OpType -> FunRes) | FFunct ([OpType] -> FunRes)
--data FunRes = NeedsRolls | Resolved OpType | Errored ResolveException
--data OpNode = OpNode {nodeDisplay::Either String (String, String, String), nodeFunction::FunType, nodeChildren::[OpType]}

toBool :: OpStatic -> Bool
toBool (StaticBool x) = x
toBool (StaticNum (GReal (GSimp (GInt 0)))) = False
toBool (StaticNum x) = True
toBool (StaticVec (OpVector [])) = False
toBool (StaticVec x) = True

opNot :: OpType -> FunRes
opNot (TypeStatic (StaticDie x)) = NeedsRolls
opNot (TypeStatic x) = Resolved $ TypeStatic $ StaticBool $ not $ toBool x
opNot a = NeedsRolls

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
opDiv (TypeStatic (StaticNum x)) (TypeStatic (StaticNum (GReal (GSimp (GInt 0))))) = Errored $ ResolveException $ T.pack "Division by Zero."
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

--addSuccess :: Dice -> (TestInput, Either GeneralNumber (GeneralNumber -> Bool)) -> String -> Dice
{-addSuccess die@Die {face=faceposs, reroll=rerolllist, suffix=suff} testTup string = die {exploding = res, suffix = suff ++ (if test then string else "")}
  where
    (res, test) = inputParse faceposs testTup rerolllist-}
--data TestInput = TestLeq Int | TestLes Int | TestGeq Int | TestGre Int | TestEq Int
--safeComp :: (GeneralRealNumber -> GeneralRealNumber -> Bool) -> GeneralNumber -> GeneralNumber -> Bool

opGre :: OpType -> OpType -> FunRes
opGre (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed to greater than."
opGre a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed to greater than."
opGre (TypeStatic (StaticNum (GReal n))) (TypeStatic (StaticNum (GReal m))) = Resolved $ TypeStatic $ StaticBool $ n > m
opGre (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Unordered number passed to greater than."
opGre a (TypeStatic (StaticVec y)) = vecComp a y (>)
opGre (TypeStatic (StaticDie d)) (TypeStatic (StaticNum n@(GReal m))) = Resolved $ TypeStatic $ StaticDie $ addSuccess d (TestGre $ floor m, Right $ safeComp (<) n) (">" ++ show n)
opGre (TypeStatic (StaticDie d)) (TypeStatic (StaticNum _)) = Errored $ ResolveException $ T.pack "Unordered number passed to greater than."
opGre a b = NeedsRolls

opLess :: OpType -> OpType -> FunRes
opLess (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed to less than."
opLess a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed to less than."
opLess (TypeStatic (StaticNum (GReal n))) (TypeStatic (StaticNum (GReal m))) = Resolved $ TypeStatic $ StaticBool $ n < m
opLess (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Unordered number passed to less than."
opLess a (TypeStatic (StaticVec y)) = vecComp a y (<)
opLess (TypeStatic (StaticDie d)) (TypeStatic (StaticNum n@(GReal m))) = Resolved $ TypeStatic $ StaticDie $ addSuccess d (TestLes $ ceiling m, Right $ safeComp (>) n) ("<" ++ show n)
opLess (TypeStatic (StaticDie d)) (TypeStatic (StaticNum _)) = Errored $ ResolveException $ T.pack "Unordered number passed to less than."
opLess a b = NeedsRolls

opGeq :: OpType -> OpType -> FunRes
opGeq (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed to greater than or equal to."
opGeq a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed to greater than or equal to."
opGeq (TypeStatic (StaticNum (GReal n))) (TypeStatic (StaticNum (GReal m))) = Resolved $ TypeStatic $ StaticBool $ n >= m
opGeq (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Unordered number passed to greater than."
opGeq a (TypeStatic (StaticVec y)) = vecComp a y (>=)
opGeq (TypeStatic (StaticDie d)) (TypeStatic (StaticNum n@(GReal m))) = Resolved $ TypeStatic $ StaticDie $ addSuccess d (TestGeq $ floor m, Right $ safeComp (<=) n) (">=" ++ show n)
opGeq (TypeStatic (StaticDie d)) (TypeStatic (StaticNum _)) = Errored $ ResolveException $ T.pack "Unordered number passed to greater than or equal to."
opGeq a b = NeedsRolls

opLeq :: OpType -> OpType -> FunRes
opLeq (TypeStatic (StaticBool x)) b = Errored $ ResolveException $ T.pack "Boolean passed to less than or equal to."
opLeq a (TypeStatic (StaticBool y)) = Errored $ ResolveException $ T.pack "Boolean passed to less than or equal to."
opLeq (TypeStatic (StaticNum (GReal n))) (TypeStatic (StaticNum (GReal m))) = Resolved $ TypeStatic $ StaticBool $ n <= m
opLeq (TypeStatic (StaticNum x)) (TypeStatic (StaticNum y)) = Errored $ ResolveException $ T.pack "Unordered number passed to less than or equal to."
opLeq a (TypeStatic (StaticVec y)) = vecComp a y (<=)
opLeq (TypeStatic (StaticDie d)) (TypeStatic (StaticNum n@(GReal m))) = Resolved $ TypeStatic $ StaticDie $ addSuccess d (TestLeq $ ceiling m, Right $ safeComp (>=) n) ("<=" ++ show n)
opLeq (TypeStatic (StaticDie d)) (TypeStatic (StaticNum _)) = Errored $ ResolveException $ T.pack "Unordered number passed to less than or equal to."
opLeq a b = NeedsRolls

opEq :: OpType -> OpType -> FunRes
--opEq (TypeStatic (StaticDie d)) (TypeStatic (StaticNum n)) = Resolved $ TypeStatic $ StaticDie $ addSuccess d (TestEq $ floor n, Right $ safeComp (>=) (GReal n)) "<="
opEq x y = Resolved $ TypeStatic $ StaticBool $ x == y

opNeq :: OpType -> OpType -> FunRes
opNeq x y = Resolved $ TypeStatic $ StaticBool $ x /= y

opIf :: [OpType] -> FunRes
opIf [] = Errored $ ResolveException $ T.pack "If called with no arguments."
opIf [x] = Errored $ ResolveException $ T.pack "If called with one argument."
opIf [x,y]
  |TypeStatic (StaticBool False) <- x = Resolved x
  |TypeStatic (StaticNum (GReal (GSimp (GInt 0)))) <- x = Resolved $ TypeStatic $ StaticBool False
  |TypeStatic (StaticVec (OpVector [])) <- x = Resolved $ TypeStatic $ StaticBool False
  |TypeStatic (StaticDie d) <- x = NeedsRolls
  |TypeStatic a <- x = Resolved y
  |otherwise = NeedsRolls
opIf (x:y:z:rest)
  |TypeStatic (StaticBool False) <- x = Resolved z
  |TypeStatic (StaticNum (GReal (GSimp (GInt 0)))) <- x = Resolved z
  |TypeStatic (StaticVec (OpVector [])) <- x = Resolved z
  |TypeStatic (StaticDie d) <- x = NeedsRolls
  |TypeStatic a <- x = Resolved y
  |otherwise = NeedsRolls

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

vecMerge :: [OpStatic] -> (GeneralNumber -> GeneralNumber -> GeneralNumber) -> FunRes
vecMerge list fun = finisher $ listMerge list
  where
    smartFun :: GeneralNumber -> Either GeneralNumber (Maybe ResolveException) -> Either GeneralNumber (Maybe ResolveException)
    smartFun n (Left m) = Left $ fun n m
    smartFun n (Right e) = Right e
    listMerge :: [OpStatic] -> Either GeneralNumber (Maybe ResolveException)
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
opSum [] = Resolved $ TypeStatic $ StaticNum $ GReal $ GSimp $ GInt 0
opSum (x:rest)
  |TypeStatic (StaticNum n) <- x = Resolved x
  |TypeStatic (StaticVec (OpVector v)) <- x = vecMerge v (+)
  |TypeStatic (StaticBool b) <- x = Errored $ ResolveException $ T.pack "Non-numeric argument passed to sum."
  |otherwise = NeedsRolls

opProd :: [OpType] -> FunRes
opProd [] = Resolved $ TypeStatic $ StaticNum $ GReal $ GSimp $ GInt 1
opProd (x:rest)
  |TypeStatic (StaticNum n) <- x = Resolved x
  |TypeStatic (StaticVec (OpVector v)) <- x = vecMerge v (*)
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

data Assoc = AssocLeft | AssocRight | AssocNone deriving (Eq, Show)
--Argument to In/Pre/Post is prec, argument to Fun is number of args
data OpTokenType = OpTokenIn Integer | OpTokenPre Integer | OpTokenPost Integer | OpTokenFun Integer | OpTokenInOrPre Integer Integer | OpTokenInOrPost Integer Integer
  deriving (Show)
data OpToken = OpToken {optype :: OpTokenType, resolveOrder :: OpResolveOrder, assoc :: Assoc, function :: FunType}

instance Show OpToken where
  show OpToken {optype = ot, assoc = a} = "OpToken: (" ++ show ot ++ ", " ++ show a ++ ")"

safeComp :: (GeneralRealNumber -> GeneralRealNumber -> Bool) -> GeneralNumber -> GeneralNumber -> Bool
safeComp fun (GReal n) (GReal m) = fun n m
safeComp fun n m = False

getType :: OpToken -> OpTokenType
getType OpToken {optype=t}=t

--rerollFun :: (Integer -> TestInput, Maybe (GeneralNumber -> GeneralNumber -> Bool), String) -> OpType -> OpType -> FunRes

operatorDict = Map.fromList
  [(T.pack "d",   OpToken {optype=OpTokenIn 12,        resolveOrder = OpResolveRight, assoc = AssocLeft,  function = FInfix dFunction 12}),
   (T.pack "#",   OpToken {optype=OpTokenIn 11,        resolveOrder = OpResolveLeftFirst,  assoc = AssocRight, function = FInfix dupFunction 11}),
   (T.pack "dF",  OpToken {optype=OpTokenPost 11,      resolveOrder = OpResolveAll,   assoc = AssocNone,  function = FUnary fudgeDie 11 Suffix}),
   (T.pack "k",   OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (keepdropFun KeepHigh) 10}),
   (T.pack "kh",  OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (keepdropFun KeepHigh) 10}),
   (T.pack "kl",  OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (keepdropFun KeepLow) 10}),
   (T.pack "dh",  OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (keepdropFun DropHigh) 10}),
   (T.pack "dl",  OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (keepdropFun DropLow) 10}),
   (T.pack "dl",  OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (keepdropFun DropLow) 10}),
   (T.pack "r",   OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (rerollFun (TestEq, Nothing, "r")) 10}),
   (T.pack "r<",  OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (rerollFun (TestLeq, Just $ safeComp (<), "r<")) 10}),
   (T.pack "r<=", OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (rerollFun (TestLeq, Just $ safeComp (<=), "r<=")) 10}),
   (T.pack "r>",  OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (rerollFun (TestLeq, Just $ safeComp (>), "r>")) 10}),
   (T.pack "r>=", OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (rerollFun (TestLeq, Just $ safeComp (>=), "r<=")) 10}),
   (T.pack "!",   OpToken {optype=OpTokenInOrPost 8 8, resolveOrder = OpResolveRight, assoc = AssocNone,  function = FPossUn (explodingFun TestEq (==) "!") 8 exclamationpoint 8 Suffix}),
   (T.pack "!<",  OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (explodingFun TestLes (safeComp (<)) "!<") 10}),
   (T.pack "!<=", OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (explodingFun TestLeq (safeComp (<=)) "!<=") 10}),
   (T.pack "!>",  OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (explodingFun TestGre (safeComp (>)) "!>") 10}),
   (T.pack "!>=", OpToken {optype=OpTokenIn 10,        resolveOrder = OpResolveRight, assoc = AssocNone,  function = FInfix (explodingFun TestGeq (safeComp (>=)) "!>=") 10}),
   (T.pack "~",   OpToken {optype=OpTokenPre 6,        resolveOrder = OpResolveAll,   assoc = AssocNone,  function = FUnary opNot 6 Prefix}),
   (T.pack "not", OpToken {optype=OpTokenPre 6,        resolveOrder = OpResolveAll,   assoc = AssocNone,  function = FUnary opNot 6 Prefix}),
   (T.pack "^",   OpToken {optype=OpTokenIn 4,         resolveOrder = OpResolveAll,   assoc = AssocRight, function = FInfix opExp 4}),
   (T.pack "**",  OpToken {optype=OpTokenIn 4,         resolveOrder = OpResolveAll,   assoc = AssocRight, function = FInfix opExp 4}),
   (T.pack "&&",  OpToken {optype=OpTokenIn 3,         resolveOrder = OpResolveAll,   assoc = AssocLeft,  function = FInfix opAdd 2}),
   (T.pack "and", OpToken {optype=OpTokenIn 3,         resolveOrder = OpResolveAll,   assoc = AssocLeft,  function = FInfix opAdd 2}),
   (T.pack "*",   OpToken {optype=OpTokenIn 3,         resolveOrder = OpResolveAll,   assoc = AssocLeft,  function = FInfix opMult 3}),
   (T.pack "/",   OpToken {optype=OpTokenIn 3,         resolveOrder = OpResolveAll,   assoc = AssocLeft,  function = FInfix opDiv 3}),
   (T.pack "%",   OpToken {optype=OpTokenIn 3,         resolveOrder = OpResolveAll,   assoc = AssocLeft,  function = FInfix opMod 3}),
   (T.pack "||",  OpToken {optype=OpTokenIn 2,         resolveOrder = OpResolveAll,   assoc = AssocLeft,  function = FInfix opOr 2}),
   (T.pack "or",  OpToken {optype=OpTokenIn 2,         resolveOrder = OpResolveAll,   assoc = AssocLeft,  function = FInfix opOr 2}),
   (T.pack "+",   OpToken {optype=OpTokenIn 2,         resolveOrder = OpResolveAll,   assoc = AssocLeft,  function = FInfix opAdd 2}),
   (T.pack "-",   OpToken {optype=OpTokenInOrPre 2 6,  resolveOrder = OpResolveAll,   assoc = AssocLeft,  function = FPossUn opMinus 2 opNeg 6 Prefix}),
   (T.pack ">",   OpToken {optype=OpTokenIn 1,         resolveOrder = OpResolveRightFirst,   assoc = AssocLeft,  function = FInfix opGre 1}),
   (T.pack "<",   OpToken {optype=OpTokenIn 1,         resolveOrder = OpResolveRightFirst,   assoc = AssocLeft,  function = FInfix opLess 1}),
   (T.pack ">=",  OpToken {optype=OpTokenIn 1,         resolveOrder = OpResolveRightFirst,   assoc = AssocLeft,  function = FInfix opGeq 1}),
   (T.pack "<=",  OpToken {optype=OpTokenIn 1,         resolveOrder = OpResolveRightFirst,   assoc = AssocLeft,  function = FInfix opLeq 1}),
   (T.pack "==",  OpToken {optype=OpTokenIn 1,         resolveOrder = OpResolveRightFirst,   assoc = AssocLeft,  function = FInfix opEq 1}),
   (T.pack "!=",  OpToken {optype=OpTokenIn 1,         resolveOrder = OpResolveRightFirst,   assoc = AssocLeft,  function = FInfix opNeq 1}),
   (T.pack "if",  OpToken {optype=OpTokenFun 3,        resolveOrder = OpResolveLeft,  assoc = AssocNone,  function = FFunct opIf}),
   (T.pack "ceil", OpToken {optype=OpTokenFun 1,       resolveOrder = OpResolveAll,   assoc = AssocNone,  function = FFunct opCeil}),
   (T.pack "round", OpToken {optype=OpTokenFun 1,      resolveOrder = OpResolveAll,   assoc = AssocNone,  function = FFunct opRound}),
   (T.pack "floor", OpToken {optype=OpTokenFun 1,      resolveOrder = OpResolveAll,   assoc = AssocNone,  function = FFunct opFloor}),
   (T.pack "max",   OpToken {optype=OpTokenFun 1,      resolveOrder = OpResolveAll,   assoc = AssocNone,  function = FFunct opMax}),
   (T.pack "min",   OpToken {optype=OpTokenFun 1,      resolveOrder = OpResolveAll,   assoc = AssocNone,  function = FFunct opMin}),
   (T.pack "sum",   OpToken {optype=OpTokenFun 1,      resolveOrder = OpResolveAll,   assoc = AssocNone,  function = FFunct opSum}),
   (T.pack "prod",  OpToken {optype=OpTokenFun 1,      resolveOrder = OpResolveAll,   assoc = AssocNone,  function = FFunct opProd})]

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
