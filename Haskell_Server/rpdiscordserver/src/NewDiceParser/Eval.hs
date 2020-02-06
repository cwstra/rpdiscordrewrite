{-# LANGUAGE OverloadedStrings #-}
module NewDiceParser.Eval
( Assoc(..)
) where

import Control.Monad

import qualified Data.Text as T

import NewDiceParser.Data
import NewDiceParser.Functions
import NewDiceParser.Monads

validateLetVec :: [Value] -> RollRes [(T.Text, Value)]
validateLetVec = return . mapM validateOne
  where
    validateOne :: Value -> Res (T.Text, Value)
    validateOne (Vector _ [Atom t, v]) = Succeeded (t, v)
    validateOne (Vector _ [_, _])      = Failed "Non-variable name passed to let definition"
    validateOne (Vector _ _)           = Failed "Vector of inproper length passed to let definition"

letSubspace :: [(T.Text, Value)] -> RollRes Value -> RollRes Value
letSubspace origL f = do
  l <- allOrNothing (\(t, v) -> (t, evaluate v)) origL
  case l of
    Succeeded v -> subSpace (mapM (uncurry putVariable) l) f
    Failed _ -> return l
  where
    allOrNothing :: (a -> RollRes b) -> [a] -> RollRes [b]
    allOrNothing f = mapM $ aONStep f
      where
        aONStep :: (a -> RollRes b) -> Res [b] -> a -> RollRes [b]
        aONStep f (Succeeded l) a = do
          res <- f a
          case res of
            Succeeded v -> return $ Succeeded (v:l)
            Failed _    -> return res
            ToDo _      -> return $ Failed "ToDo passed to allOrNothing"
        aONStep _ (ToDo _)      _ = return $ Failed "ToDo in allOrNothing"
        aONStep _ f@(Failed _)  _ = return f

letFunction :: [Value] -> RollRes Value
letFunction [Vector _ v, vs] = do
  l <- validateLetVec v
  case l of
    Succeeded vars -> letSubspace vars (evaluate vs)
    _              -> l
letFunction (Vector _ _, _) = return $ Failed "Incorrect number of arguments passed to let"
letFunction _               = return $ Failed "let did not recieve a vector as its first argument"

letStarNamespace :: [(T.Text, Value)] -> RollRes ()
letStarNamespace l = mapM (uncurry putVariable . (\(t, v) -> (t, resolve v))) l

letStarFunction :: [Value] -> RollRes Value
letStarFunction [Vector _ v, vs] = do
  l <- validateLetVec v
  case l of
    Succeeded vars -> subSpace (letStarNamespace vars) (evaluate vs)
    _              -> l

fnFunction :: [Value] -> RollRes Value
fnFunction [Vector _ v, vs] = do
  l <- validateArgs v
  case l of
    Just vars -> do
      (ns, mt) <- get
      let (mainArgs, varArgs) = checkVar
      return $ Succeeded $ Function mainArgs varArgs vs ns
    Nothing -> return $ Failed "Non-atom passed as argument to function"
  where
    validateArgs :: [Value] -> Maybe [T.Text]
    validateArgs = foldl' validateOne (Just [])
    validateOne :: Maybe [T.Text] -> Value -> [Maybe T.Text]
    validateOne (Just ls) (Atom s) = Just $ s:ls
    validateOne _         _        = Nothing
    checkVar :: [T.Text] -> ([T.Text], Maybe T.Text)
    checkVar l
      |varArg:front <- uncons l, "&":mainArgs <- uncons front = (reverse mainArgs, Just varArg)
      |otherwise                                            = (reverse l, Nothing)

data StateFunction = StateFunction
    { fText   :: T.Text
    , fType   :: [FType]
    , fAction :: ResAction
    }

igToState :: IgnorantFunction -> StateFunction
igToState ig = StateFunction
    { fText   = igText ig
    , fType   = igType ig
    , fAction = wrapIgnorant (igRes ig) (igRoll ig) (igAction ig)
    }
  where
    doOnIndex :: (Value -> RollRes Value) -> [Int] -> [Value] -> RollRes [Value]
    doOnIndex f inds l = mapM indexedF zipL
      where
        zipL = zip [0..] l
        indexedF :: (Int, Value) -> RollRes Value
        indexedF i v
          |i `elem` inds = f v
          |otherwise     = return v
    resOrder :: ResOrder -> [Value] -> RollRes [Value]
    resOrder ResolveAll        vs = mapM evaluate vs
    resOrder (ResolvePos inds) vs = doOnIndex evaluate inds vs
    resOrder ResolveNone       vs = return vs
    rollOrder :: RollOrder -> [Value] -> RollRes [Value]
    rollOrder RollAll        vs = mapM resolve vs
    rollOrder (RollPos inds) vs = doOnIndex resolve inds vs
    rollOrder RollNone       vs = return vs
    wrapIgnorant :: ResOrder -> RollOrder -> Action -> ResAction
    wrapIgnorant resO rollO a d vs = do
      resolved <- resOrder resO vs
      rolled <- rollOrder rollO vs
      return $ a vs

inlineList = map igToF igFunctionList

functionList = [ Function "let"  [FFunct] letFunction
               , Function "let*" [FFunct] letStarFunction
               , Function "fn"   [FFunct] fnFunction
               ] ++ inlineList

keyList = sortBy keyOrd $ map fText functionList
  where
    keyOrd :: T.Text -> T.Text -> Ordering
    keyOrd t1 t2
      |comparison == EQ = compare t1 t2
      |otherwise = comparison
      where
        comparison = T.compareLength t2 (T.length t1)

evaluate :: Value -> RollRes Value
evaluate value
  |Atom t <- value = do
    m <- getVariable
    case m of
      Just v -> return $ Succeeded v
      Nothing -> return $ Failed "Variable `" + t + "` is undefined"
  |Vector d (Atom f : args) = apply d f args
  |otherwise = value

apply :: ([Value] -> T.Text) -> T.Text -> RollRes Value
apply d t = do
  g <- getVariable t
  case g of
    Just v -> do
      (ns, mt) <- get
      let (f, fNS) = case v of
                   Primitive _ fun -> (fun, ns)a
                   Function p v b c -> (fromUser p v b, c
      let (res, (_, newMT)) = runStateT f (fNS, mt)
      put (ns, mt)
      return res
    Nothing -> return $ Failed "Unknown variable passed as function"

roll :: Value -> RollRes Value
roll (Atom t) = v
