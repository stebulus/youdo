module YouDo.DB.Mock where

import Prelude hiding (id)
import Control.Concurrent.MVar (MVar, withMVar, modifyMVar, newMVar)
import Data.Maybe (fromJust, listToMaybe)
import YouDo.DB (Youdo(..), DB(..))

data BareMockDB = BareMockDB { youdos :: [Youdo] }
data MockDB = MockDB { mvar :: MVar BareMockDB }

instance DB MockDB where
    getYoudo ydid db = withMVar (mvar db) $ \db ->
        return $ [yd | yd<-youdos db, maybe False (==ydid) (id yd)]
    getYoudos db = withMVar (mvar db) $ \db ->
        return $ youdos db
    postYoudo yd db = modifyMVar (mvar db) $ \db -> do
        let newid = 1 + fromJust (maybe (Just 0) id $ listToMaybe $ youdos db)
        return ( db { youdos = yd { id = Just newid } : (youdos db) }
               , newid
               )

empty :: IO MockDB
empty = do
    mv <- newMVar $ BareMockDB { youdos = [] }
    return $ MockDB mv
