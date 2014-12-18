{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module YouDo.DB.Memory (MemoryDB, empty) where
import Control.Applicative ((<$>))
import Control.Concurrent.MVar (newMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (listToMaybe)

import YouDo.DB
import YouDo.Types

-- | A transient in-memory implementation of 'DB'.
-- Note that this object is not thread-safe; wrap it in a 'LockDB' if needed.
data (Updater u v) => MemoryDB k v u
    = MemoryDB { things :: IORef [Versioned k v]
               , nextID :: IO k
               , nextTxnID :: IO TransactionID
               }

newdb :: (Updater u v) => (Int->k) -> IO TransactionID -> IO (MemoryDB k v u)
newdb toID nextTxn = do
    ioref <- newIORef []
    incr <- (fmap.fmap) toID $ newIncrementer 1
    return $ MemoryDB ioref incr nextTxn

-- Not thread-safe!
newIncrementer :: (Enum a) => a -> IO (IO a)
newIncrementer initial = do
    ioref <- newIORef initial
    return $ do
        n <- readIORef ioref
        let m = succ n
        writeIORef ioref m
        return n

instance (Eq k, NamedResource k, Updater u v)
         => DB IO k v u (MemoryDB k v u) where
    get k d = do
        xs <- readIORef $ things d
        return $ one $ [x | x<-xs, thingid (version x) == k]
    getVersions k d = do
        xs <- readIORef $ things d
        return $ some $ [x | x<-xs, thingid (version x) == k]
    getVersion vk d = do
        xs <- readIORef $ things d
        return $ one $ [x | x<-xs, version x == vk]
    getAll d = do
        xs <- readIORef $ things d
        return $ success $ nubBy ((==) `on` thingid . version) xs
    create v d = do
        newid <- nextID d
        newtxn <- nextTxnID d
        let newx = Versioned (VersionedID newid newtxn) v
        modifyIORef' (things d) (newx : )
        return $ success newx
    update vu d = do
        xs <- readIORef $ things d
        let oldx = listToMaybe [ x | x<-xs, thingid (version x) == thingid (version vu) ]
        case oldx of
            Nothing -> return notFound
            Just x -> if version x /= version vu
                        then return $ newerVersion x
                        else do newtxn <- nextTxnID d
                                let newx = Versioned
                                            (VersionedID (thingid (version x)) newtxn)
                                            (doUpdate (thing vu) (thing x))
                                modifyIORef' (things d) (newx : )
                                return $ success newx

-- | Create a new empty 'YoudoDatabase' backed by 'MemoryDB's.
-- All operations (to either 'DB') are locked under a single MVar.
empty :: IO (YoudoDatabase
                (LockDB IO YoudoID YoudoData YoudoUpdate
                    (MemoryDB YoudoID YoudoData YoudoUpdate))
                (LockDB IO UserID UserData UserUpdate
                    (MemoryDB UserID UserData UserUpdate)))
empty = do
    txnIncr <- (fmap.fmap) TransactionID $ newIncrementer 0
    mv <- newMVar ()
    yd <- LockDB mv <$> newdb YoudoID txnIncr
    ud <- LockDB mv <$> newdb UserID txnIncr
    result <- create (UserData "yddb") ud
    case result of
        Right (Right (Right _)) -> return $ YoudoDatabase yd ud
        _ -> error "could not create yddb user"
