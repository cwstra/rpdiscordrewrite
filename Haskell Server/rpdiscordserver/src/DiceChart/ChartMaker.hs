{-# LANGUAGE OverloadedStrings #-}
module DiceChart.ChartMaker where

import qualified Data.Text as T
import Data.Aeson
import Data.Functor
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

data DiceTable = DiceTable [(T.Text, [(Int, Integer)])]
{-
instance FromJSON DiceTable where
  parseJSON = withObject "DiceTable" $ \v -> DiceTable
                <$> v .: ""

makeChart :: -> FileOptions -> FilePath -> EC r () -> IO ()
-}