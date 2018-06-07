{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import qualified Data.Text.Lazy                   as T
import           Data.Word
import           System.Random.Mersenne.Pure64
import           System.Timeout
import           Web.Scotty

import           Debug.Trace
import           System.TimeIt

import qualified General.UserNumber               as UN
import           HistoryDiceParser.InfixToPostfix
import           HistoryDiceParser.PostfixToTree
import           HistoryDiceParser.ResolutionTree

site :: ScottyM ()
site = do
  get "/roll" roller
  --post

failedRoll :: Maybe T.Text -> T.Text
failedRoll Nothing  = "Resolve exception: computation timed out"
failedRoll (Just t) = t

roller :: ActionM ()
roller = do
  input <- param "roll"
  seed <- param $ trace ("input: "++ show input) "seed"
  --{-
  result <- liftAndCatchIO $ timeout 10000000 $ return $! (resShow . treeResolve . seededPostFixToTrees . seededInfixToPostfix) (input, pureMT seed)
  text $ failedRoll $ trace ("result: " ++ show result) result
  ---}
  {-
  e0 <- timeItNamed "e0" $ return $ traceShowId $! seededInfixToPostfix (input, pureMT seed)
  e1 <- timeItNamed "e1" $ return $ traceShowId $! seededPostFixToTrees e0
  e2 <- timeItNamed "e2" $ return $ traceShowId $! treeResolve e1
  e3 <- timeItNamed "e3" $ return $ traceShowId $! resShow e2
  text e3
  ---}

main :: IO ()
main = scotty 4935 site
