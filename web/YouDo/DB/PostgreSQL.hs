{-# LANGUAGE OverloadedStrings #-}
module YouDo.DB.PostgreSQL where

import Database.PostgreSQL.Simple (query, query_, execute, withTransaction,
    Only(..), Connection, Query)
import YouDo.DB (YoudoID(..), YoudoData(..), DB(..))

newtype DBConnection = DBConnection Connection
instance DB DBConnection where
    getYoudo ydid (DBConnection conn) =
        query conn
              "select id, assignerid, assigneeid, description, duedate, completed \
              \from youdo where id = ?"
              (Only ydid)

    postYoudo yd (DBConnection conn) = do
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

    getYoudos (DBConnection conn) = query_ conn
        "select id, assignerid, assigneeid, description, duedate, completed \
        \from youdo"
