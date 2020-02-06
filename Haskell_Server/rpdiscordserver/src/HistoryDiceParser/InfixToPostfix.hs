module HistoryDiceParser.InfixToPostfix(infixToPostfix, seededInfixToPostfix, strInfixToPostfix, StackTokenType(..), StackToken(..), ParseException) where

import           Data.Char
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe
import qualified Data.Text.Lazy                as T
import           System.Random.Mersenne.Pure64

import           Debug.Trace

import           General.UserNumber
import           HistoryDiceParser.Operators

--import Debug.Trace

data StackTokenType = StackTokenNum | StackTokenBool | StackTokenIn T.Text Integer | StackTokenPre T.Text Integer
                     | StackTokenPost T.Text Integer | StackTokenFun T.Text Integer | StackTokenLParen Char
                     | StackTokenSep Char | StackTokenRParen Char | StackTokenVec Integer | StackTokenRes Integer
  deriving (Show)
data StackToken = StackToken {tokenIndex :: Maybe Integer, tokenType :: StackTokenType, tokenRep :: T.Text}

instance Show StackToken where
  show StackToken {tokenType = tt, tokenRep = rep} = "Token: (\"" ++ T.unpack rep ++ "\"," ++ show tt ++ ")"

lparen = "(["
rparen = ")]"
separa = ","

multPrec = (precGet . getType . fromJust) (Map.lookup (T.singleton '*') operatorDict)
  where
    precGet :: OpTokenType -> Integer
    precGet (OpTokenIn n) = n
addPrec = (precGet . getType . fromJust) (Map.lookup (T.singleton '+') operatorDict)
  where
    precGet :: OpTokenType -> Integer
    precGet (OpTokenIn n) = n

revPre :: T.Text -> T.Text -> Bool
revPre bigstr pref = T.isPrefixOf pref bigstr

startsWithOneOf :: [T.Text] -> T.Text -> Maybe T.Text
startsWithOneOf [] text = Nothing
startsWithOneOf (x:xs) text
  |T.isPrefixOf x text = Just x
  |otherwise = startsWithOneOf xs text

startsWithFun :: T.Text -> Maybe T.Text
startsWithFun = startsWithOneOf operatorKeys

startsWithBool :: T.Text -> Maybe T.Text
startsWithBool = startsWithOneOf [T.pack "True", T.pack "False"]

startsWithEmpty :: T.Text -> Maybe T.Text
startsWithEmpty = startsWithOneOf [T.pack "()"]

getNumber :: T.Text -> Integer -> Maybe StackToken -> Maybe (Either (T.Text, Integer, StackToken) ParseException)
getNumber string index lastToken
  |Just StackToken {tokenType=StackTokenRParen c} <- lastToken = Just $ Left (string, index, StackToken {tokenIndex = Just index, tokenType = StackTokenIn (T.singleton '*') multPrec, tokenRep = T.empty})
  |otherwise = numParse string index
  where
    getDigits :: (T.Text, T.Text) -> (T.Text, T.Text)
    getDigits (pre, string) = (T.append pre (fst res), snd res)
      where
        res = T.span isDigit string
    checkFirst :: Char -> (T.Text, T.Text) -> (T.Text, T.Text)
    checkFirst c (pre, string)
      |Nothing <- tup = (pre, string)
      |Just (first, rest) <- tup, first == c =  (T.append pre (T.singleton first), rest)
      |otherwise = (pre, string)
      where
        tup = T.uncons string
    numParse :: T.Text -> Integer -> Maybe (Either (T.Text, Integer, StackToken) ParseException)
    numParse string index = Just $ Left (snd res, index + fromIntegral (T.length $ fst res),
                              StackToken {tokenIndex = Just index, tokenType = StackTokenNum, tokenRep = fst res})
      where
        res = (checkFirst 'j' . getDigits . checkFirst '.' . getDigits) (T.empty, string)

leftParen :: T.Text -> Integer -> Maybe StackToken -> Maybe (Either (T.Text, Integer, StackToken) ParseException)
leftParen string index lastToken
  |Just StackToken {tokenType=StackTokenNum} <- lastToken    = Just $ Left (string, index, StackToken {tokenIndex = Just index, tokenType = StackTokenIn (T.singleton '*') multPrec, tokenRep = T.empty})
  |Just StackToken {tokenType=StackTokenRParen c} <- lastToken = Just $ Left (string, index, StackToken {tokenIndex = Just index, tokenType = StackTokenIn (T.singleton '*') multPrec, tokenRep = T.empty})
  |Just res <- result = Just $ Left (snd res, index+1, StackToken {tokenIndex = Just index, tokenType = StackTokenLParen $ fst res, tokenRep = T.singleton $ fst res})
  |otherwise = Just $ Right $ ParseException string index
  where
    result = T.uncons string

rightParen :: T.Text -> Integer -> Maybe StackToken -> Maybe (Either (T.Text, Integer, StackToken) ParseException)
rightParen string index lastToken
  |Just res <- result = Just $ Left (snd res, index+1, StackToken {tokenIndex = Just index, tokenType = StackTokenRParen $ fst res, tokenRep = T.singleton $ fst res})
  |otherwise = Just $ Right $ ParseException string index
  where
    result = T.uncons string

separator :: T.Text -> Integer -> Maybe StackToken -> Maybe (Either (T.Text, Integer, StackToken) ParseException)
separator string index lastToken
  |Just res <- result = Just $ Left (snd res, index+1, StackToken {tokenIndex = Just index, tokenType = StackTokenSep $ fst res, tokenRep = T.singleton $ fst res})
  |otherwise = Just $ Right $ ParseException string index
  where
    result = T.uncons string

funParse :: T.Text -> Integer -> Maybe StackToken -> T.Text -> Maybe (Either (T.Text, Integer, StackToken) ParseException)
funParse string index lastToken funName
  |OpToken {optype = OpTokenInOrPre n m}  <- entry = inOrPre string index lastToken funName n m
  |OpToken {optype = OpTokenInOrPost n m} <- entry = inOrPost newstr index funName n m
  |otherwise = general string index funName
  where
    entry = fromJust $ Map.lookup funName operatorDict
    newstr = T.drop (T.length funName) string
    newind = index + fromIntegral (T.length funName)
    inOrPre :: T.Text -> Integer -> Maybe StackToken -> T.Text -> Integer -> Integer -> Maybe (Either (T.Text, Integer, StackToken) ParseException)
    inOrPre string index lastToken funName precIn precPre
      |Just StackToken {tokenType=StackTokenNum} <- lastToken      = Just $ Left (newstr, newind, StackToken {tokenIndex = Just index, tokenType = StackTokenIn  funName precIn, tokenRep = funName})
      |Just StackToken {tokenType=StackTokenRParen c} <- lastToken = Just $ Left (newstr, newind, StackToken {tokenIndex = Just index, tokenType = StackTokenIn  funName precIn, tokenRep = funName})
      |otherwise                                                   = Just $ Left (newstr, newind, StackToken {tokenIndex = Just index, tokenType = StackTokenPre funName precPre, tokenRep = funName})
    inOrPost :: T.Text -> Integer -> T.Text -> Integer -> Integer -> Maybe (Either (T.Text, Integer, StackToken) ParseException)
    inOrPost newstr index funName precIn precPost
      |Just (x, xs) <- res, isDigit x      = Just $ Left (newstr, newind, StackToken {tokenIndex = Just index, tokenType = StackTokenIn   funName precIn, tokenRep = funName})
      |otherwise                           = Just $ Left (newstr, newind, StackToken {tokenIndex = Just index, tokenType = StackTokenPost funName precPost, tokenRep = funName})
      where
        res = traceShowId $ (T.uncons . T.strip) newstr
    general :: T.Text -> Integer -> T.Text -> Maybe (Either (T.Text, Integer, StackToken) ParseException)
    general string index funName = Just $ Left (newstr, newind, StackToken {tokenIndex = Just index, tokenType = res, tokenRep = funName})
      where
        tokenTrans :: OpTokenType -> StackTokenType
        tokenTrans (OpTokenIn   n) = StackTokenIn   funName n
        tokenTrans (OpTokenPre  n) = StackTokenPre  funName n
        tokenTrans (OpTokenPost n) = StackTokenPost funName n
        tokenTrans (OpTokenFun  n) = StackTokenFun  funName n
        res = (tokenTrans . getType) entry

nextToken :: T.Text -> Integer -> Maybe StackToken -> Maybe (Either (T.Text, Integer, StackToken) ParseException)
nextToken string index lastToken
  |T.length string == 0 = Nothing
  |Just (x, xs) <- res, isSpace x                           = nextToken xs (index+1) lastToken
  |Just (x, xs) <- res, isDigit x || x=='.' || x=='j'       = getNumber string index lastToken
  |Just _ <- startsWithBool string                          = Just $ Left (T.drop 2 string,   index + 2,                StackToken {tokenIndex = Just index, tokenType = StackTokenVec 0, tokenRep = T.pack "()"})
  |Just bool <- startsWithBool string, num <- T.length bool = Just $ Left (T.drop num string, index + fromIntegral num, StackToken {tokenIndex = Just index, tokenType = StackTokenBool, tokenRep = bool})
  |Just (x, xs) <- res, x `elem` lparen                     = leftParen string index lastToken
  |Just (x, xs) <- res, x `elem` rparen                     = rightParen string index lastToken
  |Just (x, xs) <- res, x `elem` separa                     = separator string index lastToken
  |Just (x, xs) <- res, Just fun <- startsWithFun string    = funParse string index lastToken fun
  |Just (x, xs) <- res                                      = Just $ Right $ ParseException (T.singleton x) index
  |otherwise                                                = Just $ Right $ ParseException (T.singleton ' ') index
  where
    res = T.uncons string

infixToPostfix :: T.Text -> Either [StackToken] ParseException
infixToPostfix string = finisher $ infixToPostfixR string 0 [] [] (0, []) Nothing
  where
    finisher :: Either (T.Text, Integer, [StackToken]) ParseException -> Either [StackToken] ParseException
    finisher (Left (string, index, tokenList)) = Left tokenList
    finisher (Right e)                         = Right e
    infixToPostfixR :: T.Text -> Integer -> [StackToken] -> [StackToken] -> (Integer, [Integer]) -> Maybe StackToken -> Either (T.Text, Integer, [StackToken]) ParseException
    infixToPostfixR string index stack output commas lastToken
      |Just (Right e) <- res = Right e
      |Nothing <- res = infixToPostfixF string index stack output commas lastToken
      |Just (Left tup) <- res = infixToPostfixRH string index stack output commas lastToken tup
      where
        res = nextToken string index lastToken
    infixToPostfixRH :: T.Text -> Integer -> [StackToken] -> [StackToken] -> (Integer, [Integer]) -> Maybe StackToken -> (T.Text, Integer, StackToken) -> Either (T.Text, Integer, [StackToken]) ParseException
    infixToPostfixRH string index stack output commas lastToken (newString, newIndex, newToken)
      |StackToken {tokenType = StackTokenNum} <- newToken = infixToPostfixR newString newIndex stack (newToken:output) commas (Just newToken)
      |StackToken {tokenType = StackTokenBool} <- newToken = infixToPostfixR newString newIndex stack (newToken:output) commas (Just newToken)
      |StackToken {tokenType = StackTokenVec 0} <- newToken = infixToPostfixR newString newIndex stack (newToken:output) commas (Just newToken)
      |StackToken {tokenType = StackTokenFun funName prec} <- newToken = infixToPostfixR newString newIndex (newToken:stack) output commas (Just newToken)
      |StackToken {tokenType = StackTokenLParen c} <- newToken = infixToPostfixR newString newIndex (newToken:stack) output (fst commas, 0:snd commas) (Just newToken)
      |StackToken {tokenType = StackTokenRParen c} <- newToken = rParenProcess string index stack output commas lastToken (newString, newIndex, newToken)
      |StackToken {tokenType = StackTokenSep c} <- newToken,
       (n, []) <- commas,
       (newStack, newOutput) <- popTilPrec isntLParen stack output = infixToPostfixR newString newIndex newStack newOutput (n+1, []) (Just newToken)
      |StackToken {tokenType = StackTokenSep c} <- newToken,
       (m, n:ns) <- commas,
       (newStack, newOutput) <- popTilPrec isntLParen stack output = infixToPostfixR newString newIndex newStack newOutput (m, (n+1):ns) (Just newToken)
      |StackToken {tokenType = StackTokenIn name prec} <- newToken = inProcess string index stack output commas lastToken (newString, newIndex, newToken)
      |StackToken {tokenType = StackTokenPre t n} <- newToken = infixToPostfixR newString newIndex (newToken:stack) output commas (Just newToken)
      |StackToken {tokenType = StackTokenPost t n} <- newToken = postProcess string index stack output commas lastToken (newString, newIndex, newToken)
      where
        isntLParen :: StackToken -> Bool
        isntLParen StackToken {tokenType = StackTokenLParen c} = False
        isntLParen t                                           = True
        popTilPrec :: (StackToken -> Bool) -> [StackToken] -> [StackToken] -> ([StackToken], [StackToken])
        popTilPrec fun [] output = ([], output)
        popTilPrec fun stack@(x:xs) output
          |fun x = popTilPrec fun xs (x:output)
          |otherwise = (stack, output)
    infixToPostfixF :: T.Text -> Integer -> [StackToken] -> [StackToken] -> (Integer, [Integer]) -> Maybe StackToken -> Either (T.Text, Integer, [StackToken]) ParseException
    infixToPostfixF string index [] output commas lastToken = Left (string, index, output)
    infixToPostfixF string index stack@(t:ts) output commas lastToken
      |StackToken {tokenType = StackTokenLParen c} <- t,
       (m, n:ns) <- commas,
       (rep, box) <- newTokenType c = infixToPostfixF string index ts ((StackToken {tokenIndex = Nothing, tokenType = box $ offset n lastToken, tokenRep = T.pack $ rep:show (offset n lastToken)}):output) (m, ns) Nothing
      |otherwise = infixToPostfixF string index ts (t:output) commas Nothing
      where
        offset :: Integer -> Maybe StackToken -> Integer
        offset n Nothing                                         = n + 1
        offset n (Just StackToken {tokenType = StackTokenSep c}) = n
        offset n t                                               = n + 1
        newTokenType :: Char -> (Char, Integer -> StackTokenType)
        newTokenType '[' = ('r', StackTokenRes)
        newTokenType c   = ('v', StackTokenVec)
    rParenProcess :: T.Text -> Integer -> [StackToken] -> [StackToken] -> (Integer, [Integer]) -> Maybe StackToken -> (T.Text, Integer, StackToken) -> Either (T.Text, Integer, [StackToken]) ParseException
    rParenProcess string index stack output commas lastToken (newString, newIndex, newToken)
      = infixToPostfixR newString newIndex newStack newOutput newCommas (Just newToken)
      where
        newTokenType :: StackToken -> (Char, Integer -> StackTokenType)
        newTokenType StackToken {tokenType = StackTokenRParen ']'} = ('r', StackTokenRes)
        newTokenType t = ('v', StackTokenVec)
        (rep, box) = newTokenType newToken
        rParenCheck :: ([StackToken], [StackToken], (Integer, [Integer])) -> ([StackToken], [StackToken], (Integer, [Integer]))
        rParenCheck tup@([], _, _) = tup
        rParenCheck (stack@(StackToken {tokenType = StackTokenLParen c}:xs), output, commas) = (stack, output, commas)
        rParenCheck (x:xs, output, commas) = rParenCheck (xs, x:output, commas)
        getCommas :: ([StackToken], [StackToken], (Integer, [Integer])) -> (Integer, [StackToken], [StackToken], (Integer, [Integer]))
        getCommas ([], output, (n, ns))     = (n, [], output, (0, ns))
        getCommas (x:xs, output, (m, n:ns)) = (n, xs, output, (m, ns))
        makeVector ::  (Integer, [StackToken], [StackToken], (Integer, [Integer])) -> ([StackToken], [StackToken], (Integer, [Integer]))
        makeVector (0, stack, output, commas) = if rep=='r'
                                              then (stack, (StackToken {tokenIndex = Nothing, tokenType = box 1, tokenRep = T.pack [rep,'1']}):output, commas)
                                              else (stack, output, commas)
        makeVector (v, stack, output, commas) = (stack, (StackToken {tokenIndex = Nothing, tokenType = box v1, tokenRep = T.pack $ rep:show v1}):output, commas)
          where
            offset :: Integer -> StackToken -> Integer
            offset n StackToken {tokenType = StackTokenSep c} = n
            offset n t                                        = n + 1
            v1 = offset v newToken
        checkFun :: ([StackToken], [StackToken], (Integer, [Integer])) -> ([StackToken], [StackToken], (Integer, [Integer]))
        checkFun tup@([], _, _) = tup
        checkFun (token@StackToken {tokenType = StackTokenFun s n }:xs, output, commas) = (xs, token:output, commas)
        checkFun tup = tup
        (newStack, newOutput, newCommas) = (checkFun . makeVector . getCommas . rParenCheck) (stack, output, commas)
    dataGet :: StackToken -> (T.Text, Integer)
    dataGet StackToken {tokenType = StackTokenIn k prec}   = (k, prec)
    dataGet StackToken {tokenType = StackTokenPre k prec}  = (k, prec)
    dataGet StackToken {tokenType = StackTokenPost k prec} = (k, prec)
    getAssoc :: OpToken -> Assoc
    getAssoc OpToken {assoc = a} = a
    isntFun :: StackToken -> Bool
    isntFun StackToken {tokenType=StackTokenIn s n}   = False
    isntFun StackToken {tokenType=StackTokenPre s n}  = False
    isntFun StackToken {tokenType=StackTokenPost s n} = False
    isntFun t                                         = True
    inProcess :: T.Text -> Integer -> [StackToken] -> [StackToken] -> (Integer, [Integer]) -> Maybe StackToken -> (T.Text, Integer, StackToken) -> Either (T.Text, Integer, [StackToken]) ParseException
    inProcess string index stack output commas lastToken (newString, newIndex, newToken)
      |Left (newStack, newOutput) <- res = infixToPostfixR newString newIndex newStack newOutput commas (Just newToken)
      |Right e <- res = Right e
      where
        (opKey, opPrec) = dataGet newToken
        opEntry = fromJust $ Map.lookup opKey operatorDict
        opAssoc = getAssoc opEntry
        midProcess :: ([StackToken], [StackToken]) -> Either ([StackToken], [StackToken]) ParseException
        midProcess ([], output) = Left ([], output)
        midProcess (stack@(t:ts), output)
          |isntFun t = Left (stack, output)
          |opAssoc == AssocNone && otherAssoc == AssocNone && opPrec == otherPrec = Right $ ParseException (T.concat [T.pack "Operator ", opKey, T.pack " cannot be chained with ", otherKey]) newIndex
          |opPrec < otherPrec || (opAssoc == AssocLeft && opPrec == otherPrec) = midProcess (ts, t:output)
          |otherwise = Left (stack, output)
          where
            (otherKey, otherPrec) = dataGet t
            otherEntry = fromJust $ Map.lookup otherKey operatorDict
            otherAssoc = getAssoc otherEntry
        finishStack :: Either ([StackToken], [StackToken]) ParseException -> Either ([StackToken], [StackToken]) ParseException
        finishStack (Left (stack, output)) = Left (newToken:stack, output)
        finishStack (Right e)              = Right e
        res = (finishStack . midProcess) (stack, output)
    postProcess :: T.Text -> Integer -> [StackToken] -> [StackToken] -> (Integer, [Integer]) -> Maybe StackToken -> (T.Text, Integer, StackToken) -> Either (T.Text, Integer, [StackToken]) ParseException
    postProcess string index stack output commas lastToken (newString, newIndex, newToken)
      = infixToPostfixR newString newIndex newStack newOutput commas (Just newToken)
      where
        (opKey, opPrec) = dataGet newToken
        opEntry = fromJust $ Map.lookup opKey operatorDict
        opAssoc = getAssoc opEntry
        midProcess :: ([StackToken], [StackToken]) -> ([StackToken], [StackToken])
        midProcess ([], output) = ([], output)
        midProcess (stack@(t:ts), output)
          |isntFun t = (stack, output)
          |opPrec < otherPrec = midProcess (ts, t:output)
          |otherwise = (stack, output)
          where
            (_, otherPrec) = dataGet t
        finishStack :: ([StackToken], [StackToken]) -> ([StackToken], [StackToken])
        finishStack (stack, output) = (stack, newToken:output)
        (newStack, newOutput) = (finishStack . midProcess) (stack, output)

seededInfixToPostfix :: (T.Text, PureMT) -> Either ([StackToken], PureMT) ParseException
seededInfixToPostfix (text, generator)
  |Left tokens <- res = Left (tokens, generator)
  |Right e <- res = Right e
  where
    res = infixToPostfix text

strInfixToPostfix :: String -> Either [StackToken] ParseException
strInfixToPostfix str = infixToPostfix $ T.pack str
