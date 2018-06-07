module HistoryDiceParser.PostfixToTree(postFixToTrees, seededPostFixToTrees) where

import qualified Data.HashMap.Strict              as Map
import           Data.List
import           Data.Maybe
import qualified Data.Text.Lazy                   as T
import qualified Data.Text.Lazy.Read              as TR
import           System.Random.Mersenne.Pure64

--import Debug.Trace

import           General.UserNumber
import           HistoryDiceParser.InfixToPostfix
import           HistoryDiceParser.Operators
import           HistoryDiceParser.ResolutionTree

numParse :: T.Text -> GeneralNumber
numParse str
  |Just n <- res = GComp $ GC (GSimp $ GInt 0) (toNum $ intOrDec n)
  |Nothing <- res = GReal $ toNum $ intOrDec str
  where
    res = T.stripSuffix (T.singleton 'j') str
    intOrDec :: T.Text -> Either Integer Float
    intOrDec num
      |Just c <- hasP,  Right (n, rest) <- TR.rational num, rest == T.empty = Right n
      |Nothing <- hasP, Right (n, rest) <- TR.decimal num,  rest == T.empty = Left n
      where
        hasP = T.find (=='.') num
    toNum :: Either Integer Float -> GeneralRealNumber
    toNum (Left n)  = GSimp $ GInt n
    toNum (Right n) = GSimp $ GFlo n

boolParse :: T.Text -> Bool
boolParse bool
  |T.pack "True" == bool = True
  |T.pack "False" == bool = False

seededPostFixToTrees :: Either ([StackToken], PureMT) ParseException -> Either (OpType, PureMT) ParseException
seededPostFixToTrees (Left (list, generator)) = Left (postFixToTrees list, generator)
seededPostFixToTrees (Right e) = Right e

postFixToTrees :: [StackToken] -> OpType
postFixToTrees elements = finisher $ postFixToTreesR elements []
  where
    finisher :: [OpType] -> OpType
    finisher [x] = x
    finisher list = TypeNode $ OpNode {nodeDisplay = T.pack "vec", nodeFunctionKey = Nothing, nodeResOrder=OpResolveAll, nodeFunction = FFunct vectorify, nodeChildren = list}

postFixToTreesR :: [StackToken] -> [OpType] -> [OpType]
postFixToTreesR elements@[x] stack = postFixToTreesS x stack
postFixToTreesR elements@(x:xs) stack = postFixToTreesS x $ postFixToTreesR xs stack

getFun :: OpToken -> (FunType, OpResolveOrder)
getFun OpToken {function = f, resolveOrder = ord} = (f, ord)

postFixToTreesS :: StackToken -> [OpType] -> [OpType]
postFixToTreesS StackToken {tokenType = StackTokenNum, tokenRep = numStr} stack = (TypeStatic $ StaticNum $ numParse numStr):stack
postFixToTreesS StackToken {tokenType = StackTokenBool, tokenRep = boolStr} stack = (TypeStatic $ StaticBool $ boolParse boolStr):stack
postFixToTreesS StackToken {tokenType = StackTokenIn t n, tokenRep = funStr} stack@(x:y:z) = TypeNode node:z
  where
    (fun, ord) = getFun $ fromJust $ Map.lookup funStr operatorDict
    funParse :: FunType -> FunType
    funParse (FPossUn infixf infixp _ _ _) = FInfix infixf infixp
    funParse (FInfix infixf infixp)        = FInfix infixf infixp
    node = OpNode {nodeDisplay = funStr, nodeFunctionKey = Just funStr, nodeResOrder=ord, nodeFunction = funParse fun, nodeChildren = [y,x]}
postFixToTreesS StackToken {tokenType = StackTokenVec n} stack = TypeNode node:newStack
  where
    (workArgs, newStack) = genericSplitAt n stack
    args = reverse workArgs
    node = OpNode {nodeDisplay = T.pack "vec", nodeFunctionKey = Nothing, nodeResOrder=OpResolveAll, nodeFunction = FFunct opVector, nodeChildren = args}
postFixToTreesS StackToken {tokenType = StackTokenRes n} stack = TypeNode node:newStack
  where
    (workArgs, newStack) = genericSplitAt n stack
    args = reverse workArgs
    node = OpNode {nodeDisplay = T.pack "res", nodeFunctionKey = Nothing, nodeResOrder=OpResolveAll, nodeFunction = FFunct opResVector, nodeChildren = args}
postFixToTreesS StackToken {tokenType = StackTokenPre t n, tokenRep = funStr} stack@(x:y) = TypeNode node:y
  where
    (fun, ord) = getFun $ fromJust $ Map.lookup funStr operatorDict
    funParse :: FunType -> FunType
    funParse (FPossUn _ _ infixf infixp infixt) = FUnary infixf infixp infixt
    funParse (FUnary infixf infixp infixt)      = FUnary infixf infixp infixt
    node = OpNode {nodeDisplay = funStr, nodeFunctionKey = Just funStr, nodeResOrder=ord, nodeFunction = funParse fun, nodeChildren = [x]}
postFixToTreesS StackToken {tokenType = StackTokenPost t n, tokenRep = funStr} stack@(x:y) = TypeNode node:y
  where
    (fun, ord) = getFun $ fromJust $ Map.lookup funStr operatorDict
    funParse :: FunType -> FunType
    funParse (FPossUn _ _ infixf infixp infixt) = FUnary infixf infixp infixt
    funParse (FUnary infixf infixp infixt)      = FUnary infixf infixp infixt
    node = OpNode {nodeDisplay = funStr, nodeFunctionKey = Just funStr, nodeResOrder=ord, nodeFunction = funParse fun, nodeChildren = [x]}
postFixToTreesS StackToken {tokenType = StackTokenFun t n, tokenRep = funStr} stack@(x:y) = TypeNode node:y
  where
    (fun, ord) = getFun $ fromJust $ Map.lookup funStr operatorDict
    node = OpNode {nodeDisplay = funStr, nodeFunctionKey = Just funStr, nodeResOrder=ord, nodeFunction = fun, nodeChildren = [x]}
