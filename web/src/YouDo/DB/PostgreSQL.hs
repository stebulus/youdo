{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module YouDo.DB.PostgreSQL where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (newMVar)
import Database.PostgreSQL.Simple (query, query_, execute, withTransaction,
    Only(..), Connection, Query)

import YouDo.DB
import YouDo.Types

-- | Access a Youdo database over the given connection.
youdoDB :: Connection
        -> IO (YoudoDatabase IO
                (LockDB IO YoudoID YoudoData YoudoUpdate PostgresYoudoDB)
                (LockDB IO UserID UserData UserUpdate PostgresUserDB)
                (LockTxnDB IO PostgresTxnDB))
youdoDB conn = do
    mv <- newMVar ()
    return $ YoudoDatabase (LockDB mv (PostgresYoudoDB conn))
                           (LockDB mv (PostgresUserDB conn))
                           (LockTxnDB mv (PostgresTxnDB conn))

-- | A 'DB' instance backed by a PostgreSQL database.
-- Not thread-safe!  Use 'LockDB' if needed.
newtype PostgresYoudoDB = PostgresYoudoDB Connection
instance DB IO YoudoID YoudoData YoudoUpdate PostgresYoudoDB where
    get ydid (PostgresYoudoDB conn) =
        one <$> query conn
              "select id, txnid, assignerid, assigneeid, description, duedate, completed \
              \from youdo where id = ?"
              (Only ydid)
    getAll (PostgresYoudoDB conn) = success <$> query_ conn
        "select id, assignerid, assigneeid, description, duedate, completed \
        \from youdo"
    create yd (PostgresYoudoDB conn) =
        withTransaction conn $ do
            _ <- execute conn
                    ("insert into transaction (yd_userid, yd_ipaddr, yd_useragent) \
                    \values (?, ?, ?)"::Query)
                    (0::Int, "127.0.0.1"::String, "some agent"::String)
            ids <- query conn
                "insert into youdo \
                \(assignerid, assigneeid, description, duedate, completed) \
                \values (?, ?, ?, ?, ?) \
                \returning id, txnid, assignerid, assigneeid, description, duedate, completed"
                (assignerid yd, assigneeid yd, description yd,
                duedate yd, completed yd)
                :: IO [Youdo]
            return $ one ids
newtype PostgresUserDB = PostgresUserDB Connection
instance DB IO UserID UserData UserUpdate PostgresUserDB where
    get uid (PostgresUserDB conn) =
        one <$> query conn
              "select id, name from yd_user where id = ?"
              (Only uid)
    getAll (PostgresUserDB conn) = success <$> query_ conn "select name from yd_user"
    create (UserData name) (PostgresUserDB conn) =
        withTransaction conn $ do
            _ <- execute conn
                ("insert into transaction (yd_userid, yd_ipaddr, yd_useragent) \
                    \values (?,?,?)"::Query)
                (0::Int, "127.0.0.1"::String, "some agent"::String)
            users <- query conn
                "insert into yd_user \
                \(name) \
                \values (?) \
                \returning id, txnid, name"
                (Only name)
                :: IO [User]
            return $ one users

newtype PostgresTxnDB = PostgresTxnDB Connection
instance TxnDB IO PostgresTxnDB where
    getTxn tid (PostgresTxnDB conn) =
        one <$> query conn
            "select id from transaction where id = ?"
            (Only tid)
