{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module HistoryDiceParser.NewOperatorTree
( standardize
, infixOperators
, prefixOperators
, postfixOperators
, functions
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Control.Monad.Identity
import           Control.Monad.Loops
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Class
import           Data.Foldable
import qualified Data.HashMap.Strict      as HM
import           Data.List
import           Data.Maybe
import qualified Data.Text.Lazy           as T
import           Data.Word
import           System.Random.Mersenne.Pure64

import           Debug.Trace

import           General.UserNumber
import           HistoryDiceParser.NewResolveData

(<++>) = T.append

boolToMaybe :: (a -> Bool) -> a -> Maybe a
boolToMaybe p a
    |p a       = Just a
    |otherwise = Nothing

allToMaybe :: (a -> Bool) -> [a] -> Maybe [a]
allToMaybe p l
    |all p l = Just l
    |otherwise = Nothing

allMaybeToList :: (a -> Maybe b) -> [a] -> Maybe [b]
allMaybeToList p = foldr (step p) (Just [])
  where
    step :: (a -> Maybe b) -> a -> Maybe [b] -> Maybe [b]
    step p a (Just l)
        |Just b <- p a = Just $ b:l
        |otherwise = Nothing
    step _ _ Nothing = Nothing

maybeToRes :: T.Text -> Maybe a -> Resolution a
maybeToRes _ (Just a) = Resolved a
maybeToRes t _        = Failed t

isStatic :: Tree -> Bool
isStatic (TreeNum  _) = True
isStatic (TreeBool _) = True
isStatic (TreeDie  _) = True
isStatic (TreeVec  l) = all isStatic l
isStatic _            = False

checkPos :: (Num a, Ord a) => (T.Text -> T.Text) -> a -> Resolution a
checkPos dis a
    |a < 0     = Failed $ dis "Negative"
    |otherwise = Resolved a

aPosInt :: (T.Text -> T.Text) -> Tree -> Resolution Int
aPosInt dis (TreeNum n) = test n
  where
    integralTest n = maybeToRes (dis "Non-integral") $ getIntegral n
    test n =  integralTest n >>= checkPos dis
aPosInt dis _           = Failed $ dis "Non-numeric"

aNum :: (T.Text -> T.Text) -> Tree -> Resolution GeneralNumber
aNum _ (TreeNum n) = Resolved n
aNum dis _         = Failed $ dis "Non-numeric"

aVec :: (T.Text -> T.Text) -> Tree -> Resolution [Tree]
aVec _ (TreeVec l) = Resolved l
aVec dis _         = Failed $ dis "Non-vector"

aNumericVec :: (T.Text -> T.Text) -> Tree -> Resolution [GeneralNumber]
aNumericVec dis (TreeVec l)
    |Just ns <- maybeNumeric l = Resolved ns
    |otherwise                = Failed $ dis "Non-numeric vector"
  where
    isNum :: Tree -> Maybe GeneralNumber
    isNum (TreeNum n) = Just n
    isNum _                              = Nothing
    maybeNumeric :: [Tree] -> Maybe [GeneralNumber]
    maybeNumeric = allMaybeToList isNum
aNumericVec dis _         = Failed $ dis "Non-vector"

aDie :: (T.Text -> T.Text) -> Tree -> Resolution Dice
aDie _ (TreeDie d) = Resolved d
aDie dis _         = Failed $ dis "Non-die"

aFace :: (T.Text -> T.Text) -> Tree -> Resolution (Either Int [GeneralNumber])
aFace dis t = liftM Left (aPosInt dis t) <|> liftM Right (aNumericVec dis t)

generalMustBe :: (T.Text -> T.Text)
              -> Tree
              -> ((T.Text -> T.Text) -> Tree -> Resolution a)
              -> ResolveState a
generalMustBe textFun v valFun = lift $ valFun textFun v

standardize :: [[a]] -> [[a]]
standardize lists
    |maxLength == 0 = lists
    |otherwise = map pad lists
  where
    maxLength = foldl' (\m x -> max m $ length x) 0 lists
    pad :: [a] -> [a]
    pad aList@(a:_) = replicate (maxLength - length aList) a ++ aList

hisTrans :: ([History] -> History)-> [History] -> History
hisTrans f = f . transpose . standardize

parened :: (T.Text -> T.Text) -> Tree -> T.Text -> T.Text
parened fun tree text
    |TreePrefix (UnaryF {unaryParen = True}) _  <- tree = fun $ wrap text
    |TreeInfix _ (InfixF {infixParen = True}) _ <- tree = fun $ wrap text
    |TreePostfix _ (UnaryF {unaryParen = True}) <- tree = fun $ wrap text
    |otherwise    = fun text
  where
    wrap t = "(" <++> t <++> ")"

prefixHistorian :: T.Text -> Tree -> ResolveState ()
prefixHistorian rep child = historyMap_ prefixed
  where
    step :: Tree -> T.Text -> T.Text
    step = parened (rep <++>)
    prefixed :: (T.Text, T.Text) -> (T.Text, T.Text)
    prefixed (l, r) = (step child l, step child r)

infixHistorian :: T.Text -> Tree -> Tree -> ResolveState ()
infixHistorian rep child1 child2 = historyZip_ 2 infixMap
  where
    step :: Tree -> Tree -> T.Text -> T.Text -> T.Text
    step c1 c2 t1 t2 = parened id c1 t1 <++> rep <++> parened id c2 t2
    infixed :: ((T.Text, T.Text), (T.Text, T.Text)) -> (T.Text, T.Text)
    infixed ((ll, lr), (rl, rr)) = (step child1 child2 ll lr, step child1 child2 rl rr)
    toTups :: History -> ((T.Text, T.Text), (T.Text, T.Text))
    toTups [h1, h2] = (h1, h2)
    infixMap = hisTrans $ map $ infixed . toTups

postfixHistorian :: T.Text -> Tree -> ResolveState ()
postfixHistorian rep child = historyMap_ postfixed
  where
    step :: Tree -> T.Text -> T.Text
    step = parened (<++> rep)
    postfixed :: (T.Text, T.Text) -> (T.Text, T.Text)
    postfixed (l, r) = (step child l, step child r)

improperPass :: T.Text -> T.Text -> T.Text -> T.Text
improperPass p s f = p <++> " passed as " <++> s <++> " argument to " <++> f

unaryDFunction :: T.Text -> Tree -> ResolveState Tree
unaryDFunction rep child = do
    res <- simplify child >>= roll
    face <- res `mustBe` aFace
    let die = TreeDie $ createDie 1 "" face (faceShow face)
    return die
  where
    mustBe = generalMustBe $ \p -> improperPass p "right" rep
    faceShow :: Either Int [GeneralNumber] -> String
    faceShow (Left n) = show n
    faceShow (Right l) = vectorStyleShow l

binaryDFunction :: T.Text -> Tree -> Tree -> ResolveState Tree
binaryDFunction rep child1 child2 = do
    poolRes <- simplify child1 >>= roll
    pool <- poolRes `lMustBe` aPosInt
    faceRes <- simplify child2 >>= roll
    face <- faceRes `rMustBe` aFace
    let die = TreeDie $ createDie pool (show pool) face (faceShow face)
    return die
  where
    lMustBe = generalMustBe $ \p -> improperPass p "left" rep
    rMustBe = generalMustBe $ \p -> improperPass p "right" rep
    faceShow :: Either Int [GeneralNumber] -> String
    faceShow (Left n) = show n
    faceShow (Right l) = vectorStyleShow l

operatorStep :: (Int -> a -> b) -> (Int, [[b]]) -> [a] -> (Int, [[b]])
operatorStep f (i, ls) l = (i + 1, (map (f i) l):ls)

infixer :: Int -> (T.Text, Assoc, Tree -> Tree -> ResolveState Tree) -> InfixFunction
infixer i (t, a, f) = InfixF False t a i f

infixOperators = reverse $ snd $ foldl' (operatorStep infixer) (0, []) [
    []
    ]

unarier :: Int -> (T.Text, Tree -> ResolveState Tree) -> UnaryFunction
unarier i (t, f) = UnaryF False t i f

prefixOperators = reverse $ snd $ foldl' (operatorStep unarier) (0, []) [
    []
    ]

postfixOperators = reverse $ snd $ foldl' (operatorStep unarier) (0, []) [
    []
    ]

caller :: (T.Text, [Tree] -> ResolveState Tree) -> CalledFunction
caller (t, f) = CalledF t f

functions = map (map caller) [
    []
    ]
