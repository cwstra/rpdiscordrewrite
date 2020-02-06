{-# LANGUAGE OverloadedStrings #-}
module NewDiceParser.Parser
( EvalType(..)
, readExpr
) where

import Control.Monad
import Data.Char
import Data.Void

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Read
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import General.UserNumber
import NewDiceParser.Data

type Parser = Parsec Void T.Text

data EvalType = Lisp | Inline
  deriving Show

wrapResolve :: Value -> Value
wrapResolve v = Vector [atom "roll", v]

-- Shared parsing functions

char = C.char

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

symbol = L.symbol space

lexeme = L.lexeme space

atom :: Parser Value
atom = do
  c <- C.letterChar
  r <- takeWhileP Nothing $ \x -> or [isAlphaNum x, x `elem` "!#$%&|*+-/:<=>?@^_~"]
  return $ case T.cons c r of
            "True"  -> Bool True
            "False" -> Bool False
            a       -> Atom a

number :: Parser Value
number = liftM Number $ do
  r <- real
  i <- optional $ C.char 'j'
  case i of
    Just _  -> return $ r * imagUnit
    Nothing -> return $ r
  where
    integer = liftM (gReal) $ L.signed space $ lexeme L.decimal
    float   = liftM (GReal . GSimp . GFlo) $ L.signed space $ lexeme L.float
    real    = try float <|> integer

string :: Parser Value
string = do
  char '"'
  l <- manyTill escapeChar $ char '"'
  return $ String $ T.concat l
  where
    escapeChar = try slash <|> nonQuoteSlash
    slash :: Parser T.Text
    slash = liftM T.singleton $ do
      char '\\'
      c <- anySingle
      return $ case c of
                    'n' -> '\n'
                    't' -> '\t'
                    '"' -> '\"'
                    _   -> c
    nonQuoteSlash = takeWhileP Nothing $ not . (`elem` ['\\', '\"'])

bool :: Parser Value
bool = try true <|> false
  where
    true :: Parser Value
    true  = do
      chunk "True"
      return $ Bool True
    false :: Parser Value
    false = do
      chunk "False"
      return $ Bool False

parens :: T.Text -> T.Text
parens t = "(" <++> t <++> ")"

brackets :: T.Text -> T.Text
brackets t = "[" <++> t <++> "]"

-- Lisp parsing functions

lispList :: Parser [Value]
lispList = lispExpr `sepBy` space

lispVector :: Parser Value
lispVector = liftM (Vector $ lispPrint parens) $ between (symbol "(") (symbol ")") lispList

lispResVector :: Parser Value
lispResVector = liftM ( . Vector (lispPrint brackets)) $ between (symbol "[") (symbol "]") lispList

quoted :: Parser Value
quoted = do
  C.char '\''
  x <- lispExpr
  return $ Vector (lispPrint parens) [Atom "quote", x]

lispExpr :: Parser Value
lispExpr = try atom
       <|> try bool
       <|> try number
       <|> try string
       <|> try quoted
       <|> try lispVector
       <|> try lispResVector

-- Inline parsing functions

inlineList :: Parser [Value]
inlineList = number `sepBy` (symbol ",")
--inlineList = inlineExpression `sepBy` (symbol ',')

inlineVector :: Parser Value
inlineVector = liftM (Vector inlineParenPrint) $ between (symbol "(") (symbol ")") inlineList

inlineResVector :: Parser Value
inlineResVector = liftM (wrapResolve . Vector inlineBracketPrint) $ between (symbol "[") (symbol "]") inlineList

-- General expression

expression :: EvalType -> Parser Value
expression Lisp = lispExpr

-- reader

readExpr :: EvalType -> T.Text -> Either String Value
readExpr evaltype input
  |Left err <- res = Left $ errorBundlePretty err
  |Right val <- res = Right val
  where
    res = parse (expression evaltype) (show evaltype) input
