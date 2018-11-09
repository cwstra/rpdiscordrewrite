
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
import           HistoryDiceParser.NewResolveData

spec :: Spec
spec = do
  describe "HistoryDiceParser.NewOperators" $ do

    describe "" $ do

