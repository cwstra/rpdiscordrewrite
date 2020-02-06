{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module DiceChart.DataGeneral(

) where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ServerData
  members T.Text
  stats [T.Text]
  deriving Show

MemberData
  stats [T.Text]
  deriving Show

RollData
  stats T.Text
  deriving Show
|]

connStr = "host=localhost dbname=rollstats user=rpbot password=genericrollplayingpassword port=5432"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        john <- get "1"
        liftIO $ print (john :: Maybe ServerData)

{-
serverData :: Table (Text :*: Text :*: Text)
serverData = table "server_roll_stats" $ primary "id" :*: required "members" :*: required "stats"

memberData :: Table (Text :*: [Text])
memberData = table "member_roll_stats" $ primary "id" :*: required "stats"

serverRollData :: Table (Text :*: Text)
serverRollData = table "server_pool_roll_stats" $ primary "id" :*: required "stats"
memberRollData :: Table (Text :*: Text)
memberRollData = table "server_pool_roll_stats" $ primary "id" :*: required "stats"
-}

--HM.HashMap (Word, Word) (HM.HashMap Word Word)
