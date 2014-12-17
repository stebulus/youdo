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

{-
    Youdos
-}

type Youdo = Versioned YoudoID YoudoData
instance FromRow Youdo where
    fromRow = Versioned <$> (VersionedID <$> field <*> field)
        <*> (YoudoData <$> field <*> field <*> field <*> field <*> field)

newtype YoudoID = YoudoID Int deriving (Eq)
instance NamedResource YoudoID where
    resourceName = const "youdos"
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

data YoudoData = YoudoData { assignerid :: UserID
                           , assigneeid :: UserID
                           , description :: String
                           , duedate :: DueDate
                           , completed :: Bool
                           } deriving (Show)
instance RequestParsable YoudoData where
    template = YoudoData <$> parse "assigner"
                         <*> parse "assignee"
                         <*> parse "description" `defaultTo` ""
                         <*> parse "duedate" `defaultTo` DueDate Nothing
                         <*> parse "completed" `defaultTo` False
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

data YoudoUpdate = YoudoUpdate { newAssignerid :: Maybe UserID
                               , newAssigneeid :: Maybe UserID
                               , newDescription :: Maybe String
                               , newDuedate :: Maybe DueDate
                               , newCompleted :: Maybe Bool
                               } deriving (Show)
instance RequestParsable YoudoUpdate where
    template = YoudoUpdate <$> optional (parse "assigner")
                           <*> optional (parse "assignee")
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

{-
    Users
-}

type User = Versioned UserID UserData
instance FromRow User where
    fromRow = Versioned <$> (VersionedID <$> field <*> field)
                        <*> (UserData <$> field)

newtype UserID = UserID Int deriving (Eq)
instance NamedResource UserID where
    resourceName = const "users"
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

data UserData = UserData { name :: String }
    deriving (Show, Eq)
instance RequestParsable UserData where
    template = UserData <$> parse "name"
instance BasedToJSON UserData where
    basedToJSON yduser = return $ object [ "name" .= name yduser ]

data UserUpdate = UserUpdate { newName :: Maybe String } deriving (Show, Eq)
instance RequestParsable UserUpdate where
    template = UserUpdate <$> optional (parse "name")
instance Updater UserUpdate UserData where
    doUpdate upd u = case newName upd of
        Nothing -> u
        Just n -> u { name = n }

{-
    IDs in general
-}

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

{-
    The duedate field, as a newtype mostly to avoid orphan instances.
-}

newtype DueDate = DueDate { toMaybeTime :: Maybe UTCTime } deriving (Eq, Show)
instance Parsable DueDate where
    parseParam "" = Right $ DueDate Nothing
    parseParam t = either (Left . LT.pack)
                          (Right . DueDate . Just)
                          $ parseUTCTime t
instance BasedParsable DueDate where
    basedParseParam = flip $ const parseParam
instance ToJSON DueDate where
    toJSON (DueDate Nothing) = Null
    toJSON (DueDate (Just t)) = toJSON t
instance FromJSON DueDate where
    parseJSON Null = DueDate <$> pure Nothing
    parseJSON (String t) = either fail return $
        DueDate . Just <$> parseUTCTime (LT.fromStrict t)
    parseJSON x = typeMismatch "String or Null" x
instance BasedFromJSON DueDate where
    basedParseJSON = flip $ const parseJSON
instance FromField DueDate where
    fromField fld = (fmap.fmap) DueDate $ fromField fld
instance ToField DueDate where
    toField (DueDate t) = toField t
