{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module YouDo.DB.Memory where
import Control.Concurrent.MVar (MVar, withMVar, modifyMVar, newMVar)
import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (listToMaybe)

import YouDo.DB
import YouDo.Types

data (Updater u v) => MemoryDB k v u
    = MemoryDB { mvar :: MVar [Versioned k v]
               , nextID :: IO k
               , nextTxnID :: IO TransactionID
               }

newdb :: (Updater u v) => (Int->k) -> IO TransactionID -> IO (MemoryDB k v u)
newdb toID nextTxn = do
    mv <- newMVar []
    incr <- (fmap.fmap) toID $ newIncrementer 1
    return $ MemoryDB mv incr nextTxn

newIncrementer :: (Enum a) => a -> IO (IO a)
newIncrementer initial = do
    mv <- newMVar initial
    return $ modifyMVar mv $ \n -> return (succ n, n)

instance (Eq k, NamedResource k, Updater u v)
         => DB IO k v u (MemoryDB k v u) where
    get k d = withMVar (mvar d) $ \xs ->
        return $ one $ [x | x<-xs, thingid (version x) == k]
    getVersions k d = withMVar (mvar d) $ \xs ->
        return $ some $ [x | x<-xs, thingid (version x) == k]
    getVersion vk d = withMVar (mvar d) $ \xs ->
        return $ one $ [x | x<-xs, version x == vk]
    getAll d = withMVar (mvar d) $ \xs ->
        return $ success $ nubBy ((==) `on` thingid . version) xs
    create v d = modifyMVar (mvar d) $ \xs -> do
        newid <- nextID d
        newtxn <- nextTxnID d
        let newx = Versioned (VersionedID newid newtxn) v
        return (newx : xs, success newx)
    update vu d = modifyMVar (mvar d) $ \xs -> do
        let oldx = listToMaybe [ x | x<-xs, thingid (version x) == thingid (version vu) ]
        case oldx of
            Nothing -> return (xs, notFound)
            Just x -> if version x /= version vu
                        then return (xs, newerVersion x)
                        else do newtxn <- nextTxnID d
                                let newx = Versioned
                                            (VersionedID (thingid (version x)) newtxn)
                                            (doUpdate (thing vu) (thing x))
                                return (newx:xs, success newx)

empty :: IO (YoudoDatabase (MemoryDB YoudoID YoudoData YoudoUpdate)
                           (MemoryDB UserID UserData UserUpdate))
empty = do
    txnIncr <- (fmap.fmap) TransactionID $ newIncrementer 0
    yd <- newdb YoudoID txnIncr
    ud <- newdb UserID txnIncr
    result <- create (UserData "yddb") ud
    case result of
        Right (Right (Right _)) -> return $ YoudoDatabase yd ud
        _ -> error "could not create yddb user"
