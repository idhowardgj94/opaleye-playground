{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Opaleye (Field, FieldNullable, matchNullable, isNull,
                Table, table, tableField, selectTable,
                Select, restrict, (.==), (.<=), (.&&), (.<),
                (.===),
                (.++), ifThenElse, sqlString, aggregate, groupBy,
                count, avg, sum, leftJoin, runSelect,
                showSql, viaLateral, Unpackspec,
                SqlInt4, SqlInt8, SqlText, SqlDate, SqlFloat8, SqlBool)
import Data.Profunctor.Product (p2, p3, p6)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product.Default (Default)
import qualified Database.PostgreSQL.Simple as PGS
import Data.Maybe
import Database.PostgreSQL.Simple (ConnectInfo(connectPassword))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . fromMaybe "Empty select" . showSql

connectTestDB :: IO PGS.Connection
connectTestDB = PGS.connect PGS.defaultConnectInfo 
                            { PGS.connectDatabase = "postgres"
                            , PGS.connectUser = "postgres"
                            , PGS.connectPassword = "example"
                            }
