{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses #-}
module YouDo.Types where

import Control.Applicative ((<$>), (<*>), Applicative(..))
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), object, Value(..))
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as LT
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Network.URI (parseURI, relativeFrom, URI)
import Web.Scotty (Parsable(..))

import YouDo.DB
import YouDo.TimeParser (parseUTCTime)
import YouDo.Web

data ( DB YoudoID YoudoData YoudoUpdate IO yd
     , DB UserID UserData UserUpdate IO ud
     ) => YoudoDatabase yd ud = YoudoDatabase { youdos :: yd, users :: ud }

type Youdo = Versioned YoudoID YoudoData
instance NamedResource YoudoID where
    resourceName = const "youdos"
instance FromRow Youdo where
    fromRow = Versioned <$> (VersionedID <$> field <*> field)
        <*> (YoudoData <$> field <*> field <*> field <*> field <*> field)
instance BasedToJSON YoudoData where
    basedToJSON yd = do
        assigner <- basedToJSON $ assignerid yd
        assignee <- basedToJSON $ assigneeid yd
        return $ object
            [ "assigner" .= assigner
            , "assignee" .= assignee
            , "description" .= description yd
            , "duedate" .= duedate yd
            , "completed" .= completed yd
            ]

newtype YoudoID = YoudoID Int deriving (Eq)
instance Show YoudoID where
    show (YoudoID n) = show n
instance BasedToJSON YoudoID where
    basedToJSON = idjson
instance BasedFromJSON YoudoID where
    basedParseJSON val base =
        fmap YoudoID $ basedIDFromJSON val resourcebase
        where resourcebase = resourceBaseURL (Nothing :: Maybe YoudoID) base
instance BasedParsable YoudoID where
    basedParseParam txt base =
        fmap YoudoID $ basedIDFromText txt resourcebase
        where resourcebase = resourceBaseURL (Nothing :: Maybe YoudoID) base
instance FromField YoudoID where
    fromField fld = (fmap.fmap) YoudoID $ fromField fld
instance ToField YoudoID where
    toField (YoudoID n) = toField n
instance FromJSON YoudoID where
    parseJSON x = YoudoID <$> parseJSON x
instance Parsable YoudoID where
    parseParam x = YoudoID <$> parseParam x
instance FromParam Int YoudoID where
    fromParam = YoudoID
instance (Constructor f) => Constructible (f YoudoID) where
    construct = YoudoID <$> parse "id"

data YoudoData = YoudoData { assignerid :: UserID
                           , assigneeid :: UserID
                           , description :: String
                           , duedate :: DueDate
                           , completed :: Bool
                           } deriving (Show)
instance (Constructor f) => Constructible (f YoudoData) where
    construct = YoudoData <$> parse "assignerid"
                          <*> parse "assigneeid"
                          <*> parse "description" `defaultTo` ""
                          <*> parse "duedate" `defaultTo` DueDate Nothing
                          <*> parse "completed" `defaultTo` False

data YoudoUpdate = YoudoUpdate { newAssignerid :: Maybe UserID
                               , newAssigneeid :: Maybe UserID
                               , newDescription :: Maybe String
                               , newDuedate :: Maybe DueDate
                               , newCompleted :: Maybe Bool
                               } deriving (Show)
instance (Constructor f) => Constructible (f YoudoUpdate) where
    construct = YoudoUpdate <$> optional (parse "assignerid")
                            <*> optional (parse "assigneeid")
                            <*> optional (parse "description")
                            <*> optional (parse "duedate")
                            <*> optional (parse "completed")

instance Updater YoudoUpdate YoudoData where
    doUpdate upd yd =
        yd { assignerid = fromMaybe (assignerid yd) (newAssignerid upd)
           , assigneeid = fromMaybe (assigneeid yd) (newAssigneeid upd)
           , description = fromMaybe (description yd) (newDescription upd)
           , duedate = fromMaybe (duedate yd) (newDuedate upd)
           , completed = fromMaybe (completed yd) (newCompleted upd)
           }

type User = Versioned UserID UserData
instance NamedResource UserID where
    resourceName = const "users"
instance FromRow User where
    fromRow = Versioned <$> (VersionedID <$> field <*> field)
                        <*> (UserData <$> field)
instance BasedToJSON UserData where
    basedToJSON yduser = return $ object [ "name" .= name yduser ]

newtype UserID = UserID Int deriving (Eq)
instance Show UserID where
    show (UserID n) = show n
instance BasedToJSON UserID where
    basedToJSON = idjson
instance BasedFromJSON UserID where
    basedParseJSON val base = do
        fmap UserID $ basedIDFromJSON val resourcebase
        where resourcebase = resourceBaseURL (Nothing :: Maybe UserID) base
instance BasedParsable UserID where
    basedParseParam txt base =
        fmap UserID $ basedIDFromText txt resourcebase
        where resourcebase = resourceBaseURL (Nothing :: Maybe UserID) base
instance FromField UserID where
    fromField fld = (fmap.fmap) UserID $ fromField fld
instance ToField UserID where
    toField (UserID n) = toField n
instance FromJSON UserID where
    parseJSON x = UserID <$> parseJSON x
instance Parsable UserID where
    parseParam x = UserID <$> parseParam x
instance FromParam Int UserID where
    fromParam = UserID
instance (Constructor f) => Constructible (f UserID) where
    construct = UserID <$> parse "id"

basedIDFromJSON :: Value -> URI -> Parser Int
basedIDFromJSON (String txt) base =
    case basedIDFromText (LT.fromStrict txt) base of
        Left msg -> fail $ LT.unpack msg
        Right n -> return n
basedIDFromJSON val _ = typeMismatch "ID URL" val

basedIDFromText :: LT.Text -> URI -> Either LT.Text Int
basedIDFromText txt base = do
    uri <- maybe (Left "invalid URL") Right $ parseURI (LT.unpack txt)
    case reads $ show $ uri `relativeFrom` base of
        (n,""):_ -> Right n
        (n,"/"):_ -> Right n
        _ -> Left "invalid ID"

data UserData = UserData { name :: String }
    deriving (Show, Eq)
instance (Constructor f) => Constructible (f UserData) where
    construct = UserData <$> parse "name"

data UserUpdate = UserUpdate { newName :: Maybe String } deriving (Show, Eq)

instance (Constructor f) => Constructible (f UserUpdate) where
    construct = UserUpdate <$> optional (parse "name")

instance Updater UserUpdate UserData where
    doUpdate upd u = case newName upd of
        Nothing -> u
        Just n -> u { name = n }

-- This newtype avoids orphan instances.
newtype DueDate = DueDate { toMaybeTime :: Maybe UTCTime } deriving (Eq, Show)
instance Parsable DueDate where
    parseParam "" = Right $ DueDate Nothing
    parseParam t = either (Left . LT.pack)
                          (Right . DueDate . Just)
                          $ parseUTCTime t
instance ToJSON DueDate where
    toJSON (DueDate Nothing) = Null
    toJSON (DueDate (Just t)) = toJSON t
instance FromJSON DueDate where
    parseJSON Null = DueDate <$> pure Nothing
    parseJSON (String t) = either fail return $
        DueDate . Just <$> parseUTCTime (LT.fromStrict t)
    parseJSON x = typeMismatch "String or Null" x

instance FromField DueDate where
    fromField fld = (fmap.fmap) DueDate $ fromField fld
instance ToField DueDate where
    toField (DueDate t) = toField t
