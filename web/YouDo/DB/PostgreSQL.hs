{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module YouDo.DB.PostgreSQL where

import Database.PostgreSQL.Simple (query, query_, execute, withTransaction,
    Only(..), Connection, Query)
import YouDo.DB

newtype DBConnection = DBConnection Connection
instance DB YoudoID YoudoData IO DBConnection where
    get ydid (DBConnection conn) =
        query conn
              "select id, txnid, assignerid, assigneeid, description, duedate, completed \
              \from youdo where id = ?"
              (Only ydid)
    post yd (DBConnection conn) = do
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
instance DB UserID UserData IO DBConnection where
    get uid (DBConnection conn) =
        query conn
              "select id, name from yd_user where id = ?"
              (Only uid)

instance YoudoDB DBConnection where
    getYoudos (DBConnection conn) = query_ conn
        "select id, assignerid, assigneeid, description, duedate, completed \
        \from youdo"
