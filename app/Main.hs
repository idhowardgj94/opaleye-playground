{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib
import Opaleye (Field, FieldNullable, matchNullable, isNull,
                Table, table, tableField, selectTable,
                Select, restrict, (.==), (.<=), (.&&), (.<),
                (.===),
                (.++), ifThenElse, sqlString, aggregate, groupBy,
                count, avg, sum, leftJoin, runSelect,
                showSql, viaLateral, Unpackspec,
                SqlInt4, SqlInt8, SqlText, SqlDate, SqlFloat8, SqlBool, SqlTimestamp)
import Data.Profunctor.Product (p2, p3, p6)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product.Default (Default)
import qualified Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.Time (Date)
import Data.Time

-- a test command
-- connectTestDB >>= (\ t -> runTodoSelect t todoSelect)
-- let's define an todoList table function.
-- Table type consturctor has two params, which define what we can read and write.
-- to keep it simple at first, I just keep them the same.
todoListTable :: Table (Field SqlInt4,
                        Field SqlText, 
                        Field SqlBool, 
                        Field SqlTimestamp, 
                        Field SqlText, 
                        FieldNullable SqlText)
                       (Field SqlInt4,
                        Field SqlText, 
                        Field SqlBool, 
                        Field SqlTimestamp, 
                        Field SqlText, 
                        FieldNullable SqlText)
-- noted that we use p6 combinator here, it define in Data.Productor.Product
todoListTable = table "todo_lists" (p6 ( tableField "id"
                                        , tableField "topic"
                                        , tableField "is_done"
                                        , tableField "created_time"
                                        , tableField "content"
                                        , tableField "ps"))
todoSelect :: Select (Field SqlInt4,
                      Field SqlText, 
                      Field SqlBool, 
                      Field SqlTimestamp, 
                      Field SqlText, 
                      FieldNullable SqlText)
todoSelect = selectTable todoListTable 

runTodoSelect :: PGS.Connection
               ->  Select (Field SqlInt4,
                           Field SqlText, 
                           Field SqlBool, 
                           Field SqlTimestamp, 
                           Field SqlText, 
                           FieldNullable SqlText)
              -> IO [(Int, String, Bool, LocalTime, String, Maybe String)]
runTodoSelect = runSelect
main :: IO ()
main = do
    putStrLn "let play something fun!"
