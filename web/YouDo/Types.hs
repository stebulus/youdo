{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses,
    FunctionalDependencies #-}
module YouDo.Types where
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), object, Value(..))
import Data.Aeson.Types (parseEither)
import Data.Default
import Data.String (IsString(..))
import qualified Data.Text.Lazy as LT
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Web.Scotty (Parsable(..))
import YouDo.Holex

-- d contains versioned key value pairs (k,v), in monad m
class (Monad m, NamedResource k)
      => DB k v u m d | d->v, d->k, d->u, d->m where
    get :: k -> d -> m [Versioned k v]
    getVersion :: VersionedID k -> d -> m [Versioned k v]
    getVersions :: k -> d -> m [Versioned k v]
    getAll :: d -> m [Versioned k v]
    post :: v -> d -> m k
    update :: u -> d -> m (UpdateResult k)
    dbResourceName :: d -> Maybe k -> String
    dbResourceName _ x = resourceName x

data VersionedID a = VersionedID
    { thingid :: a
    , txnid :: TransactionID
    } deriving (Show, Eq)
instance (IsString k, Eq k, Parsable a, FromJSON a)
         => Default (Holex k ParamValue (VersionedID a)) where
    def = VersionedID <$> parse "id" <*> parse "txnid"

data Versioned a b = Versioned
    { version :: VersionedID a
    , thing :: b
    } deriving (Show, Eq)

class NamedResource a where
    resourceName :: Maybe a -> String

newtype TransactionID = TransactionID Int deriving (Eq)
instance Show TransactionID where
    show (TransactionID n) = show n
instance FromField TransactionID where
    fromField fld = (fmap.fmap) TransactionID $ fromField fld
instance Parsable TransactionID where
    parseParam x = TransactionID <$> parseParam x
instance FromJSON TransactionID where
    parseJSON x = TransactionID <$> parseJSON x

type Youdo = Versioned YoudoID YoudoData
instance NamedResource YoudoID where
    resourceName = const "youdos"
instance FromRow Youdo where
    fromRow = Versioned <$> (VersionedID <$> field <*> field)
        <*> (YoudoData <$> field <*> field <*> field <*> field <*> field)
instance ToJSON YoudoData where
    toJSON yd = object
        [ "assignerid" .= assignerid yd
        , "assigneeid" .= assigneeid yd
        , "description" .= description yd
        , "duedate" .= duedate yd
        , "completed" .= completed yd
        ]

newtype YoudoID = YoudoID Int deriving (Eq)
instance Show YoudoID where
    show (YoudoID n) = show n
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
instance (IsString k, Eq k) => Default (Holex k ParamValue YoudoData) where
    def = YoudoData <$> parse "assignerid"
                    <*> parse "assigneeid"
                    <*> defaultTo "" (parse "description")
                    <*> defaultTo (DueDate Nothing) (parse "duedate")
                    <*> defaultTo False (parse "completed")

data YoudoUpdate = YoudoUpdate { oldVersion :: VersionedID YoudoID
                               , newAssignerid :: Maybe UserID
                               , newAssigneeid :: Maybe UserID
                               , newDescription :: Maybe String
                               , newDuedate :: Maybe DueDate
                               , newCompleted :: Maybe Bool
                               } deriving (Show)
instance (IsString k, Eq k) => Default (Holex k ParamValue YoudoUpdate) where
    def = YoudoUpdate <$> (VersionedID <$> parse "id"
                                       <*> parse "txnid")
                      <*> optional (parse "assignerid")
                      <*> optional (parse "assigneeid")
                      <*> optional (parse "description")
                      <*> optional (parse "duedate")
                      <*> optional (parse "completed")

parse :: (Eq k, Parsable a, FromJSON a) => k -> Holex k ParamValue a
parse k = tryApply
    (Const (\x ->
        case x of
            ScottyParam txt ->
                case parseParam txt of
                    Left err -> Left (ParseError k x err)
                    Right val -> Right val
            JSONField jsonval ->
                case parseEither parseJSON jsonval of
                    Left err -> Left (ParseError k x (LT.pack err))
                    Right val -> Right val))
    $ hole k

data ParamValue = ScottyParam LT.Text
                | JSONField Value
    deriving (Eq, Show)

type User = Versioned UserID UserData
instance NamedResource UserID where
    resourceName = const "users"
instance FromRow User where
    fromRow = Versioned <$> (VersionedID <$> field <*> field)
                        <*> (UserData <$> field)
instance ToJSON UserData where
    toJSON yduser = object [ "name" .= name yduser ]

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
instance (IsString k, Eq k) => Default (Holex k ParamValue UserData) where
    def = UserData <$> parse "name"

data UserUpdate = UserUpdate { oldUserVersion :: VersionedID UserID
                             , newName :: Maybe String
                             } deriving (Show, Eq)
instance (IsString k, Eq k) => Default (Holex k ParamValue UserUpdate) where
    def = UserUpdate <$> (VersionedID <$> parse "id"
                                      <*> parse "txnid")
                     <*> optional (parse "name")

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
