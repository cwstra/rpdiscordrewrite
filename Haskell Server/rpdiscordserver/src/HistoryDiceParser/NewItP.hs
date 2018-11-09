module HistoryDiceParser.NewItP
(Tree(..)
, expr) where

import           Control.Monad.Combinators
import           Control.Monad.Combinators.Expr
import           Control.Monad.State.Lazy
import           Data.Char
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe
import qualified Data.Text.Lazy                as T
import           Data.Void
import           System.Random.Mersenne.Pure64
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Debug.Trace

import           General.UserNumber
import           HistoryDiceParser.Operators

-- Defining the Parser (Change String to T.Text later)
type Parser = Parsec Void String

-- Defining constants

lparen  = '('
rparen  = ')'
lbrack  = '['
rbrack  = ']'
separa  = ','
trueSt  = "True"
falseSt = "False"
  {-
parserOperators = sortPrecList $ toPrecList $ Map.toList operatorDict
  where
    toPrecList :: [(T.Text, OpToken)] -> ([(Integer, Operator (Parser Tree))], [Parser Tree])
    toPrecList [] = []
    toPrecList ((OpToken {optype = OpTokenFun _}):xs) = precStep x ++ toPrecList xs
    toPrecList (x:xs) = precStep x ++ toPrecList xs
      where
        binaryN name f = InfixN  (f <$ symbol name)
        binaryL name f = InfixL  (f <$ symbol name)
        binaryR name f = InfixR  (f <$ symbol name)
        prefix  name f = Prefix  (f <$ symbol name)
        postfix name f = Postfix (f <$ symbol name)
        precStep :: (T.Text, OpToken) -> [(Integer, Operator (Parser Tree))]
        precStep (opText, originalToken@(OpToken {optype=ot, assoc = oa}))
          |OpTokenIn n                <- ot
          |OpTokenPre n               <- ot
          |OpTokenPost n              <- ot
          |OpTokenInOrPre iPrec pPrec <- ot
    sortPrecList :: [(Integer, Operator)] -> [[Operator]]
-}
-- defining abstract parsers i.e. those used in other parsers, but not the end product directly

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty $ L.skipBlockComment "|*" "*|"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

enclosed :: Char -> Char -> Parser a -> Parser a
enclosed l r = between (char l) (char r)

parens   :: Parser a -> Parser a
parens   = enclosed lparen rparen
brackets :: Parser a -> Parser a
brackets = enclosed lbrack rbrack

listOf :: Parser a -> Parser [a]
listOf a = a `sepBy` (char separa)

-- defining base parsers

numbParse :: Parser Tree
numbParse = do
  res <- try imag <|> try imunit <|> try float <|> integer
  return $ TreeNum res
  where
    imag   = do
        res <- try float <|> try integer
        im  <- imunit
        return $ res * im
    imunit = do
        single 'j'
        return $ imagUnit 
    integer = do
        res <- lexeme L.decimal
        return $ gReal res 
    float   = do 
        res <- lexeme L.float
        return $ GReal $ GSimp $ GFlo res 
boolParse :: Parser Tree
boolParse = trueParse <|> falseParse
  where
    trueParse  = do
                  string "True"
                  return $ TreeBool True
    falseParse = do
                  string "False"
                  return $ TreeBool False

-- defining compound parsers

expr = try vec <|> try res <|> constant

constant :: Parser Tree
constant = try boolParse <|> numbParse

vec :: Parser Tree
vec = do
    list <- parens $ listOf expr
    return $ TreeVec list
res :: Parser Tree
res = do
    list <- brackets $ listOf expr
    return $ TreeRes list

