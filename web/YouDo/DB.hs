{-# LANGUAGE OverloadedStrings #-}
module YouDo.DB where
import Prelude hiding (id)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

data Youdo = Youdo { id :: Maybe Int  -- Nothing for new Youdos
                   , assignerid :: Int
                   , assigneeid :: Int
                   , description :: String
                   , duedate :: Maybe UTCTime
                   , completed :: Bool
                   } deriving (Show)

instance FromRow Youdo where
    fromRow = Youdo <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON Youdo where
    toJSON yd = object
        [ "id" .= id yd
        , "assignerid" .= assignerid yd
        , "assigneeid" .= assigneeid yd
        , "description" .= description yd
        , "duedate" .= duedate yd
        , "completed" .= completed yd
        ]

class DB a where
    getYoudo :: Int -> a -> IO [Youdo]
    postYoudo :: Youdo -> a -> IO Int
    getYoudos :: a -> IO [Youdo]
