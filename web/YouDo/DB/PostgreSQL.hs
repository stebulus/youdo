{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module YouDo.DB.PostgreSQL where

import Database.PostgreSQL.Simple (query, query_, execute, withTransaction,
    Only(..), Connection, Query)
import YouDo.DB

newtype PostgresYoudoDB = PostgresYoudoDB Connection
instance DB YoudoID YoudoData YoudoUpdate IO PostgresYoudoDB where
    get ydid (PostgresYoudoDB conn) =
        query conn
              "select id, txnid, assignerid, assigneeid, description, duedate, completed \
              \from youdo where id = ?"
              (Only ydid)
    post yd (PostgresYoudoDB conn) = do
        withTransaction conn $ do
            _ <- execute conn
                    ("insert into transaction (yd_userid, yd_ipaddr, yd_useragent) \
                    \values (?, ?, ?)"::Query)
                    (0::Int, "127.0.0.1"::String, "some agent"::String)
            ids <- query conn
                "insert into youdo \
                \(assignerid, assigneeid, description, duedate, completed) \
                \values (?, ?, ?, ?, ?) returning id"
                (assignerid yd, assigneeid yd, description yd,
                duedate yd, completed yd)
                :: IO [Only YoudoID]
            return $ fromOnly $ head ids
newtype PostgresUserDB = PostgresUserDB Connection
instance DB UserID UserData UserUpdate IO PostgresUserDB where
    get uid (PostgresUserDB conn) =
        query conn
              "select id, name from yd_user where id = ?"
              (Only uid)

newtype PostgresDB = PostgresDB Connection
instance YoudoDB PostgresDB where
    getYoudos (PostgresDB conn) = query_ conn
        "select id, assignerid, assigneeid, description, duedate, completed \
        \from youdo"
