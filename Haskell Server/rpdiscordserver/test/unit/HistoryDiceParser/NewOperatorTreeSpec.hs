
module HistoryDiceParser.NewOperatorTreeSpec where

import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State.Lazy
import           Data.Aeson
import qualified Data.ByteString                  as B
import           Data.Char
import qualified Data.HashMap.Strict              as HM
import           Data.List
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy                   as T
import           System.Random.Mersenne.Pure64
import           Test.Hspec
import           Test.QuickCheck

import           Debug.Trace

import           General.UserNumber
import           HistoryDiceParser.NewOperatorTree

nonEmptyString :: Gen String
nonEmptyString = listOf1 arbitrary

spec :: Spec
spec = do
  describe "HistoryDiceParser.NewOperatorTree" $ do

    describe "standardize" $ do

      it "makes a list of lists square" $ forAll (listOf nonEmptyString) $
        \lOL -> let maxLength = foldl' (\m l -> max m $ length l) 0 lOL
                in all (\l -> length l == maxLength) (standardize lOL)

