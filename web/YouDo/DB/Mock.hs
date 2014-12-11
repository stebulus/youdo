module YouDo.DB.Mock where

import Prelude hiding (id)
import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, withMVar, modifyMVar, newMVar)
import Data.Maybe (listToMaybe)
import YouDo.DB

data BareMockDB = BareMockDB
    { youdos :: [Youdo]
    , users :: [YoudoUser]
    , lasttxn :: TransactionID
    }
data MockDB = MockDB { mvar :: MVar BareMockDB }

instance DB MockDB where
    getYoudo ydid db = withMVar (mvar db) $ \db' ->
        return $ [yd | yd<-youdos db', youdoid (version yd) == ydid]
    getYoudoVersions ydid db = withMVar (mvar db) $ \db' ->
        return $ [yd | yd<-youdos db', youdoid (version yd) == ydid]
    getYoudoVersion ydver db = withMVar (mvar db) $ \db' ->
        return $ [yd | yd<-youdos db', version yd == ydver]
    getYoudos db = withMVar (mvar db) $ \db' ->
        return $ youdos db'
    postYoudo yd db = modifyMVar (mvar db) $ \db' -> do
        let newid = YoudoID $
                1 + case youdoid . version <$> (listToMaybe $ youdos db') of
                        Nothing -> 0
                        Just (YoudoID n) -> n
            newtxn = TransactionID $
                1 + case lasttxn db' of TransactionID n -> n
        return ( db' { youdos = Youdo
                        { version = (YoudoVersionID newid newtxn)
                        , youdo = yd
                        } : (youdos db')
                     , lasttxn = newtxn
                     }
               , newid
               )
    updateYoudo upd db = modifyMVar (mvar db) $ \db' -> do
        let oldyd = listToMaybe
                [yd | yd<-youdos db'
                    , youdoid (version yd) == youdoid (oldVersion upd)]
        case oldyd of
            Nothing -> return (db', Failure $ "no youdo with " ++ (show $ oldVersion upd))
            Just theyd -> if version theyd /= oldVersion upd
                            then return (db', OldVersion $ version theyd)
                            else let newyd = Youdo { version = YoudoVersionID
                                                        (youdoid (version theyd))
                                                        newtxn
                                                   , youdo = doUpdate upd (youdo theyd)
                                                   }
                                     newtxn = TransactionID $
                                         1 + case lasttxn db' of TransactionID n -> n
                                 in return (db' { youdos = newyd : (youdos db')
                                                , lasttxn = newtxn
                                                }
                                           , Success $ version newyd
                                           )
    getUser uid db = withMVar (mvar db) $ \db' ->
        return $ [u | u<-users db', userid (userVersion u) == uid]
    getUserVersion verid db = withMVar (mvar db) $ \db' ->
        return $ [u | u<-users db', userVersion u == verid]
    getUserVersions verid db = withMVar (mvar db) $ \db' ->
        return $ [u | u<-users db', userid (userVersion u) == verid]

doUpdate :: YoudoUpdate -> YoudoData -> YoudoData
doUpdate upd yd =
    (\yd -> case newAssignerid upd of
                Nothing -> yd
                Just assigner -> yd { assignerid = assigner })
    $ (\yd -> case newAssigneeid upd of
                Nothing -> yd
                Just assignee -> yd { assigneeid = assignee })
    $ (\yd -> case newDescription upd of
                Nothing -> yd
                Just descr -> yd { description = descr })
    $ (\yd -> case newDuedate upd of
                Nothing -> yd
                Just dd -> yd { duedate = dd })
    $ (\yd -> case newCompleted upd of
                Nothing -> yd
                Just c -> yd { completed = c })
    yd

empty :: IO MockDB
empty = do
    mv <- newMVar $ BareMockDB
            { youdos = []
            , users = [YoudoUser (UserVersionID (UserID 0)
                                                (TransactionID 0))
                                 (YoudoUserData "yddb")]
            , lasttxn = TransactionID 0
            }
    return $ MockDB mv
