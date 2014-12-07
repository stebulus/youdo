module YouDo.DB.Mock where

import Prelude hiding (id)
import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, withMVar, modifyMVar, newMVar)
import Data.Maybe (listToMaybe)
import YouDo.DB (YoudoID(..), YoudoVersionID(..), Youdo(..), TransactionID(..),
    DB(..))

data BareMockDB = BareMockDB { youdos :: [Youdo], lasttxn :: TransactionID }
data MockDB = MockDB { mvar :: MVar BareMockDB }

instance DB MockDB where
    getYoudo ydid db = withMVar (mvar db) $ \db' ->
        return $ [yd | yd<-youdos db', youdoid (version yd) == ydid]
    getYoudoVersions ydid db = withMVar (mvar db) $ \db' ->
        return $ [version yd | yd<-youdos db', youdoid (version yd) == ydid]
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

empty :: IO MockDB
empty = do
    mv <- newMVar $ BareMockDB { youdos = [], lasttxn = TransactionID 0 }
    return $ MockDB mv
