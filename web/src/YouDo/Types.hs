{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses, RankNTypes, ScopedTypeVariables  #-}
module YouDo.Types where

import Control.Applicative ((<$>), (<*>), Applicative(..))
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT(..), left, right)
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
import YouDo.Web.Response

data ( DB m YoudoID YoudoData YoudoUpdate yd
     , DB m UserID UserData UserUpdate ud
     , TxnDB m td
     )
  => YoudoDatabase m yd ud td
  = YoudoDatabase { youdos :: yd
                  , users :: ud
                  , transactions :: td
                  }

mapYoudoDB :: ( DB m YoudoID YoudoData YoudoUpdate yd
              , DB m UserID UserData UserUpdate ud
              , TxnDB m td
              , Monad n
              )
           => (forall a. m a -> n a)
           -> YoudoDatabase m yd ud td
           -> YoudoDatabase n
                    (LiftedDB m n YoudoID YoudoData YoudoUpdate yd)
                    (LiftedDB m n UserID UserData UserUpdate ud)
                    (LiftedTxnDB m n td)
mapYoudoDB f db = YoudoDatabase { youdos = mapDB f (youdos db)
                                , users = mapDB f (users db)
                                , transactions = mapTxnDB f (transactions db)
                                }

data TxnDeluxe = TxnDeluxe Transaction [Youdo] [User]

instance BasedToJSON TxnDeluxe where
    basedToJSON (TxnDeluxe txn ys us) uri = augmentObject (basedToJSON txn uri)
        [ "youdos" .= basedToJSON ys uri
        , "users" .= basedToJSON us uri
        ]

getTxnDeluxe :: ( DB m YoudoID YoudoData YoudoUpdate y
                , DB m UserID UserData UserUpdate u
                , TxnDB m t
                )
             => TransactionID -> YoudoDatabase m y u t
             -> m (GetResult TxnDeluxe)
getTxnDeluxe k (YoudoDatabase yd ud td) =
        return . either id success =<< meith
    where meith = runEitherT $ do
            txn <- hoistGetResult $ getTxn k td
            ys <- hoistGetResult $ getFromTxn k yd
            us <- hoistGetResult $ getFromTxn k ud
            return $ TxnDeluxe txn ys us

hoistGetResult :: (Monad m)
               => m (GetResult a) -> EitherT (GetResult b) m a
hoistGetResult act = do
    result <- lift act
    case result of
        Left x -> left (Left x)
        Right (Left x) -> left (Right (Left x))
        Right (Right x) -> right x

{-
    Transactions
-}

data Transaction = Transaction { ownid :: TransactionID
                               , timestamp :: UTCTime
                               }
instance FromRow Transaction where
    fromRow = Transaction <$> field <*> field
instance BasedToJSON Transaction where
    basedToJSON txn uri =
        object [ "url" .= idjson (ownid txn) uri
               , "timestamp" .= timestamp txn
               ]

-- | A read-only store of transaction information.
class (Monad m) => TxnDB m d where
    -- | Get the specified transaction.
    getTxn :: TransactionID -> d -> m (GetResult Transaction)

-- | Change results of a 'TxnDB' from one monad to another.
mapTxnDB :: (Monad n, TxnDB m d)
         => (forall a. m a -> n a) -> d -> LiftedTxnDB m n d
mapTxnDB = LiftedTxnDB

data LiftedTxnDB m n d = LiftedTxnDB (forall a. m a -> n a) d

instance (Monad n, TxnDB m d) => TxnDB n (LiftedTxnDB m n d) where
    getTxn t (LiftedTxnDB f d) = f $ getTxn t d

-- | A 'TxnDB' wrapper that locks on a given 'MVar'.
data (TxnDB m d) => LockTxnDB m d = LockTxnDB (MVar ()) d

instance (MonadIO m, TxnDB m d) => TxnDB m (LockTxnDB m d) where
    getTxn t (LockTxnDB mv d) =
        join $ liftIO $ withMVar mv $ const $ return $ getTxn t d

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
    basedParseJSON val uri =
        fmap YoudoID $ basedIDFromJSON val resourcebase
        where resourcebase = resourceBaseURL (Nothing :: Maybe YoudoID) uri
instance BasedParsable YoudoID where
    basedParseParam txt uri =
        fmap YoudoID $ basedIDFromText txt resourcebase
        where resourcebase = resourceBaseURL (Nothing :: Maybe YoudoID) uri
instance FromField YoudoID where
    fromField fld = (fmap.fmap) YoudoID $ fromField fld
instance ToField YoudoID where
    toField (YoudoID n) = toField n
instance Parsable YoudoID where
    parseParam x = YoudoID <$> parseParam x
instance HasCaptureDescr YoudoID where
    captureDescr = const $ "Youdo ID, as decimal integer"

data YoudoData = YoudoData { assignerid :: UserID
                           , assigneeid :: UserID
                           , description :: String
                           , duedate :: DueDate
                           , completed :: Bool
                           } deriving (Show)
instance (Applicative f, FromRequestBodyContext f)
         => FromRequestBody f YoudoData where
    template = YoudoData <$> parse "assigner"
                         <*> parse "assignee"
                         <*> parse "description" `defaultTo` ""
                         <*> parse "duedate" `defaultTo` DueDate Nothing
                         <*> parse "completed" `defaultTo` False
instance BasedToJSON YoudoData where
    basedToJSON yd uri =
        object
            [ "assigner" .= assigner
            , "assignee" .= assignee
            , "description" .= description yd
            , "duedate" .= duedate yd
            , "completed" .= completed yd
            ]
        where assigner = basedToJSON (assignerid yd) uri
              assignee = basedToJSON (assigneeid yd) uri

data YoudoUpdate = YoudoUpdate { newAssignerid :: Maybe UserID
                               , newAssigneeid :: Maybe UserID
                               , newDescription :: Maybe String
                               , newDuedate :: Maybe DueDate
                               , newCompleted :: Maybe Bool
                               } deriving (Show)
instance (Applicative f, FromRequestBodyContext f)
         => FromRequestBody f YoudoUpdate where
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
    basedParseJSON val uri = do
        fmap UserID $ basedIDFromJSON val resourcebase
        where resourcebase = resourceBaseURL (Nothing :: Maybe UserID) uri
instance BasedParsable UserID where
    basedParseParam txt uri =
        fmap UserID $ basedIDFromText txt resourcebase
        where resourcebase = resourceBaseURL (Nothing :: Maybe UserID) uri
instance HasJSONDescr UserID where
    jsonDescr = const $ "string containing URL referring to user"
instance FromField UserID where
    fromField fld = (fmap.fmap) UserID $ fromField fld
instance ToField UserID where
    toField (UserID n) = toField n
instance Parsable UserID where
    parseParam x = UserID <$> parseParam x
instance HasCaptureDescr UserID where
    captureDescr = const $ "User ID, as decimal integer"

data UserData = UserData { name :: String }
    deriving (Show, Eq)
instance (Applicative f, FromRequestBodyContext f)
         => FromRequestBody f UserData where
    template = UserData <$> parse "name"
instance BasedToJSON UserData where
    basedToJSON yduser _ = object [ "name" .= name yduser ]

data UserUpdate = UserUpdate { newName :: Maybe String } deriving (Show, Eq)
instance (Functor f, FromRequestBodyContext f)
         => FromRequestBody f UserUpdate where
    template = UserUpdate <$> optional (parse "name")
instance Updater UserUpdate UserData where
    doUpdate upd x = case newName upd of
        Nothing -> x
        Just n -> x { name = n }

{-
    IDs in general
-}

basedIDFromJSON :: Value -> URI -> Parser Int
basedIDFromJSON (String txt) uri =
    case basedIDFromText (LT.fromStrict txt) uri of
        Left msg -> fail $ LT.unpack msg
        Right n -> return n
basedIDFromJSON val _ = typeMismatch "ID URL" val

basedIDFromText :: LT.Text -> URI -> Either LT.Text Int
basedIDFromText txt uri = do
    iduri <- maybe (Left "invalid URL") Right $ parseURI (LT.unpack txt)
    case reads $ show $ iduri `relativeFrom` uri of
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
instance HasJSONDescr DueDate where
    jsonDescr = const $ "string in format yyyy-mm-ddThh:mm:ss.sssZ, or null"
instance FromField DueDate where
    fromField fld = (fmap.fmap) DueDate $ fromField fld
instance ToField DueDate where
    toField (DueDate t) = toField t
