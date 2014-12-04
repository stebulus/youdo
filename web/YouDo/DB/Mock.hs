module YouDo.DB.Mock where

import Prelude hiding (id)
import Control.Concurrent.MVar (MVar, withMVar, modifyMVar, newMVar)
import Data.Maybe (listToMaybe)
import YouDo.DB (YoudoID(..), Youdo(..), DB(..))

data BareMockDB = BareMockDB { youdos :: [Youdo] }
data MockDB = MockDB { mvar :: MVar BareMockDB }

instance DB MockDB where
    getYoudo ydid db = withMVar (mvar db) $ \db' ->
        return $ [yd | yd<-youdos db', id yd == ydid]
    getYoudos db = withMVar (mvar db) $ \db' ->
        return $ youdos db'
    postYoudo yd db = modifyMVar (mvar db) $ \db' -> do
        let newid = YoudoID $
                1 + case listToMaybe $ youdos db' of
                        Nothing -> 0
                        Just (Youdo { id = YoudoID n }) -> n
        return ( db' { youdos = Youdo { id = newid, youdo = yd } : (youdos db') }
               , newid
               )

empty :: IO MockDB
empty = do
    mv <- newMVar $ BareMockDB { youdos = [] }
    return $ MockDB mv
