{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts,
    TypeSynonymInstances, MultiParamTypeClasses #-}
module YouDo.DB where
import Prelude hiding (id)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), object, Value(..))
import qualified Data.Text.Lazy as LT
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Web.Scotty (Parsable(..))

-- d contains versioned key value pairs (k,v), in monad m
class (Monad m) => DB k v m d where
    get :: k -> d -> m [Versioned k v]
    getVersion :: VersionedID k -> d -> m [Versioned k v]
    getVersions :: k -> d -> m [Versioned k v]

class ( DB YoudoID YoudoData IO a
      , DB UserID UserData IO a)
      => YoudoDB a where
    postYoudo :: YoudoData -> a -> IO YoudoID
    updateYoudo :: YoudoUpdate -> a -> IO (UpdateResult YoudoID)
    getYoudos :: a -> IO [Youdo]

data VersionedID a = VersionedID
    { thingid :: a
    , txnid :: TransactionID
    } deriving (Show, Eq)

data Versioned a b = Versioned
    { version :: VersionedID a
    , thing :: b
    } deriving (Show, Eq)

newtype TransactionID = TransactionID Int deriving (Show, Eq)
instance FromField TransactionID where
    fromField fld = (fmap.fmap) TransactionID $ fromField fld
instance Parsable TransactionID where
    parseParam x = TransactionID <$> parseParam x
instance FromJSON TransactionID where
    parseJSON x = TransactionID <$> parseJSON x

type Youdo = Versioned YoudoID YoudoData
instance FromRow Youdo where
    fromRow = Versioned <$> (VersionedID <$> field <*> field)
        <*> (YoudoData <$> field <*> field <*> field <*> field <*> field)
instance ToJSON Youdo where
    toJSON yd = object
        [ "id" .= thingid (version yd)
        , "assignerid" .= assignerid (thing yd)
        , "assigneeid" .= assigneeid (thing yd)
        , "description" .= description (thing yd)
        , "duedate" .= duedate (thing yd)
        , "completed" .= completed (thing yd)
        ]

newtype YoudoID = YoudoID Int deriving (Show, Eq)
instance FromField YoudoID where
    fromField fld = (fmap.fmap) YoudoID $ fromField fld
instance ToField YoudoID where
    toField (YoudoID n) = toField n
instance ToJSON YoudoID where
    toJSON (YoudoID n) = toJSON n
instance FromJSON YoudoID where
    parseJSON x = YoudoID <$> parseJSON x
instance Parsable YoudoID where
    parseParam x = YoudoID <$> parseParam x

data YoudoData = YoudoData { assignerid :: UserID
                           , assigneeid :: UserID
                           , description :: String
                           , duedate :: DueDate
                           , completed :: Bool
                           } deriving (Show)

data YoudoUpdate = YoudoUpdate { oldVersion :: VersionedID YoudoID
                               , newAssignerid :: Maybe UserID
                               , newAssigneeid :: Maybe UserID
                               , newDescription :: Maybe String
                               , newDuedate :: Maybe DueDate
                               , newCompleted :: Maybe Bool
                               } deriving (Show)

type User = Versioned UserID UserData
instance FromRow User where
    fromRow = Versioned <$> (VersionedID <$> field <*> field)
                        <*> (UserData <$> field)
instance ToJSON User where
    toJSON yduser = object
        [ "id" .= thingid (version yduser)
        , "name" .= name (thing yduser)
        ]

newtype UserID = UserID Int deriving (Show, Eq)
instance FromField UserID where
    fromField fld = (fmap.fmap) UserID $ fromField fld
instance ToField UserID where
    toField (UserID n) = toField n
instance ToJSON UserID where
    toJSON (UserID n) = toJSON n
instance FromJSON UserID where
    parseJSON x = UserID <$> parseJSON x
instance Parsable UserID where
    parseParam x = UserID <$> parseParam x

data UserData = UserData { name :: String }
    deriving (Show, Eq)

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
instance FromJSON DueDate where
    parseJSON x = DueDate <$> (possibleDate <$> parseJSON x)
        where possibleDate s = if s == ""
                                then Nothing
                                else parseISO8601 s
instance FromField DueDate where
    fromField fld = (fmap.fmap) DueDate $ fromField fld
instance ToField DueDate where
    toField (DueDate t) = toField t

data UpdateResult a = Success (VersionedID a)
                    | Failure String
                    | OldVersion (VersionedID a)
