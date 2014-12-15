{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses #-}
module YouDo.Types where

import Control.Applicative ((<$>), (<*>), Applicative(..), (<*))
import qualified Control.Applicative as A
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), object, Value(..),
    withText)
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Attoparsec.Text.Lazy (decimal, char, endOfInput)
import qualified Data.Attoparsec.Text.Lazy as Atto
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Network.URI (parseURI, relativeFrom)
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
    basedParseJSON val = do
        baseuri <- resourceBaseURL (Nothing :: Maybe UserID)
        let fromJust :: String -> Maybe a -> Parser a
            fromJust msg Nothing = fail msg
            fromJust _ (Just x) = return x
            nonempty :: String -> [x] -> Parser [x]
            nonempty msg [] = fail msg
            nonempty _ xs = return xs
            f :: ST.Text -> Parser UserID
            f s = do
                uri <- fromJust "invalid URL" $ parseURI (ST.unpack s)
                useridstr <- nonempty "invalid URL" $ show $ uri `relativeFrom` baseuri
                case Atto.parse
                         (UserID <$> decimal <* (A.optional $ char '/') <* endOfInput)
                         (LT.pack useridstr)
                        of
                    Atto.Done "" result -> return result
                    Atto.Fail unconsumed stack msg -> fail $ intercalate " / " $
                        stack ++ [msg, "next characters: " ++ (take 10 $ LT.unpack unconsumed)]
                    _ -> error "impossible; endOfInput consumes everything"
        lift $ withText "UserID URL" f val
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
