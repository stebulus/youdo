{-# LANGUAGE OverloadedStrings #-}
module YouDo.DB where
import Prelude hiding (id)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (ToJSON(..), (.=), object, Value(..))
import qualified Data.Text.Lazy as LT
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Web.Scotty (Parsable(..))

newtype YoudoID = YoudoID Int deriving (Show, Eq)
instance FromField YoudoID where
    fromField fld = (fmap.fmap) YoudoID $ fromField fld
instance ToField YoudoID where
    toField (YoudoID n) = toField n
instance ToJSON YoudoID where
    toJSON (YoudoID n) = toJSON n
instance Parsable YoudoID where
    parseParam x = YoudoID <$> parseParam x

newtype TransactionID = TransactionID Int deriving (Show, Eq)
instance FromField TransactionID where
    fromField fld = (fmap.fmap) TransactionID $ fromField fld
instance Parsable TransactionID where
    parseParam x = TransactionID <$> parseParam x

data YoudoVersionID = YoudoVersionID
    { youdoid :: YoudoID
    , youdotxnid :: TransactionID
    } deriving (Show, Eq)

newtype UserID = UserID Int deriving (Show, Eq)
instance FromField UserID where
    fromField fld = (fmap.fmap) UserID $ fromField fld
instance ToField UserID where
    toField (UserID n) = toField n
instance ToJSON UserID where
    toJSON (UserID n) = toJSON n
instance Parsable UserID where
    parseParam x = UserID <$> parseParam x

data Youdo = Youdo { version :: YoudoVersionID
                   , youdo :: YoudoData
                   } deriving (Show)

data YoudoData = YoudoData { assignerid :: UserID
                           , assigneeid :: UserID
                           , description :: String
                           , duedate :: DueDate
                           , completed :: Bool
                           } deriving (Show)

data YoudoUpdate = YoudoUpdate { oldVersion :: YoudoVersionID
                               , newAssignerid :: Maybe UserID
                               , newAssigneeid :: Maybe UserID
                               , newDescription :: Maybe String
                               , newDuedate :: Maybe DueDate
                               , newCompleted :: Maybe Bool
                               } deriving (Show)

-- This newtype avoids orphan instances.
newtype DueDate = DueDate { toMaybeTime :: Maybe UTCTime } deriving (Show)
instance Parsable DueDate where
    parseParam "" = Right $ DueDate Nothing
    parseParam t = case parseISO8601 (LT.unpack t) of
        Nothing -> Left $ LT.concat ["could not parse date ", t]
        Just x -> Right $ DueDate $ Just x
instance ToJSON DueDate where
    toJSON (DueDate Nothing) = Null
    toJSON (DueDate (Just t)) = toJSON t
instance FromField DueDate where
    fromField fld = (fmap.fmap) DueDate $ fromField fld
instance ToField DueDate where
    toField (DueDate t) = toField t

instance FromRow Youdo where
    fromRow = Youdo <$> (YoudoVersionID <$> field <*> field)
        <*> (YoudoData <$> field <*> field <*> field <*> field <*> field)

instance ToJSON Youdo where
    toJSON yd = object
        [ "id" .= youdoid (version yd)
        , "assignerid" .= assignerid (youdo yd)
        , "assigneeid" .= assigneeid (youdo yd)
        , "description" .= description (youdo yd)
        , "duedate" .= duedate (youdo yd)
        , "completed" .= completed (youdo yd)
        ]

class DB a where
    getYoudo :: YoudoID -> a -> IO [Youdo]
    getYoudoVersion :: YoudoVersionID -> a -> IO [Youdo]
    getYoudoVersions :: YoudoID -> a -> IO [YoudoVersionID]
    postYoudo :: YoudoData -> a -> IO YoudoID
    updateYoudo :: YoudoUpdate -> a -> IO YoudoUpdateResult
    getYoudos :: a -> IO [Youdo]

data YoudoUpdateResult = Success YoudoVersionID
                       | Failure String
                       | OldVersion YoudoVersionID
