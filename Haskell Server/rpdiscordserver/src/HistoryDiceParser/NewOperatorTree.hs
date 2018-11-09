module HistoryDiceParser.NewOperatorTree
( FunctionType(..)
, Operator(..)
, Tree(..)
, infixOperators
, unaryOperators
, functions
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
import           HistoryDiceParser.NewResolveData

data FunctionType = FUnary (GenInput -> Resolution)
                  | FInfix (GenInput -> GenInput -> Resolution)
                  | FFunct ([GenInput] -> Resolution)

data Operator = Operator {opFun :: FunctionType, opName :: T.Text}

instance Show Operator where
    show (Operator {opName = t}) = T.unpack t

data Tree = TreeNum     GeneralNumber
          | TreeBool    Bool
          | TreePrefix  OpToken Tree
          | TreeInfix   Tree Operator Tree
          | TreePostfix Tree Operator
          | TreeFun     Operator Tree
          | TreeVec     [Tree]
          | TreeRes     [Tree]

treeRecShow :: Int -> Tree -> String
treeRecShow n t = replicate n ' ' ++ treeStep t
  where
    next  = treeRecShow $ n + 2
    lined = intercalate "\n"
    mid p l = lined $ [p ++ show (length l)] ++ map next l
    vecDrop :: Show a => String -> String -> String -> [a] -> String
    vecDrop left right pre list = concat [left, mid pre list, right]
    treeStep :: Tree -> String
    treeStep (TreeNum n)         = show n
    treeStep (TreeBool b)        = show b
    treeStep (TreePrefix o t)    = show o ++ "\n" ++ next t
    treeStep (TreeInfix t1 o t2) = lined $ [show o] ++ map next [t1 t2]
    treeStep (TreePostfix t o)   = show o ++ "\n" ++ next t
    treeStep (TreeFun o t)       = show o ++ "\n" ++ next t
    treeStep (TreeVec l)         = vecDrop "(" ")" "vec" l
    treeStep (TreeRes l)         = vecDrop "[" "]" "res" l

instance Show Tree where
    show = treeRecShow 0

data GenInput = InStat   Static
              | TreeStat Tree

instance Show GenInput where
    show (InStat s)   = show s
    show (TreeStat t) = show t


