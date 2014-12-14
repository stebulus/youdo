{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts,
    FlexibleInstances, UndecidableInstances #-}
{-|
Module      : YouDo.WebApp
Description : Web application for YouDo
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.WebApp (
    -- * Web application
    app, webdb,
    -- * URLs for resources
    resourceURL, resourceVersionURL
) where

import Codec.MIME.Type (mimeType, MIMEType(Application))
import Codec.MIME.Parse (parseMIMEType)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad (liftM)
import Control.Monad.Error (mapErrorT, throwError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ReaderT(..), mapReaderT)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import Data.Default
import Data.List (foldl', intercalate)
import Data.String (IsString(..))
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (ok200, created201, badRequest400, notFound404,
    methodNotAllowed405, conflict409, unsupportedMediaType415,
    internalServerError500, Status, StdMethod(..))
import Network.URI (URI(..), relativeTo, nullURI)
import Web.Scotty (ScottyM, matchAny, header, addroute, params,
    ActionM, Parsable(..), body)
import qualified Web.Scotty as Scotty
import Web.Scotty.Internal.Types (ActionT(..), ActionError(..),
    ScottyError(..))

import YouDo.DB
import YouDo.Holex
import YouDo.Types
import YouDo.Web

-- | The Scotty application.
-- Consists of 'webdb' interfaces for the given Youdo and User DB instances.
app :: ( DB YoudoID YoudoData YoudoUpdate IO ydb
       , DB UserID UserData UserUpdate IO udb
       ) => YoudoDatabase ydb udb     -- ^The database.
       -> MVar ()       -- ^All database access is under this MVar.
       -> Based ScottyM ()
app db mv =
    local ("./0/" `relative`) $ do
        webdb mv (youdos db)
        webdb mv (users db)

-- | A web interface to an instance of 'DB'.
-- The following endpoints are created, relative to the given base URI
-- (which should probably end with a slash):
--
-- @
--      GET objs                (list of all current objs)
--      POST objs               (create new obj)
--      GET objs\//id/\/             (current version of obj)
--      GET objs\//id/\/versions    (all versions of obj)
--      GET objs\//id/\//txnid/       (specified version of obj)
--      POST objs\//id/\//txnid/      (create new version of obj)
-- @
--
-- These correspond directly to the methods of 'DB'.  The name @objs@
-- is obtained from the instance 'NamedResource' @k@.  Requests that
-- return objects return them in JSON format, using the instances
-- 'Show' @k@ and 'ToJSON' @v@.  The @/id/@ parameter is interpreted
-- via the instance 'Parsable' @k@.  (The 'FromJSON' @k@ instance
-- would only be used if the id were passed in the JSON request
-- body, which it shouldn't be.)  The request body, when needed,
-- is interpreted via default 'RequestParser' for the appropriate type.
webdb :: ( NamedResource k, DB k v u IO d
         , Parsable k, A.FromJSON k
         , Show k, ToJSON v
         , Default (RequestParser k)
         , Default (RequestParser v)
         , Default (RequestParser (Versioned k u))
         ) => MVar ()     -- ^All database access is under this MVar.
         -> d           -- ^The database.
         -> Based ScottyM ()
webdb mv db = do
    let rtype = dbResourceName db
        onweb f = webfunc $ lock mv $ flip f db
    resource rtype
             [ (GET, onweb (\() -> getAll))
             , (POST, onweb create)
             ]
    resource (rtype ++ "/:id/")
             [ (GET, onweb get) ]
    resource (rtype ++ "/:id/versions")
             [ (GET, onweb getVersions) ]
    resource (rtype ++ "/:id/:txnid")
             [ (GET, onweb getVersion)
             , (POST, onweb update)
             ]

-- | The relative URL for a 'NamedResource' object.
resourceRelativeURLString :: (Show k, NamedResource k) => k -> String
resourceRelativeURLString k = "./" ++ resourceName (Just k) ++ "/"
                              ++ show k ++ "/"

-- | The URL for a 'NamedResource' object.
resourceURL :: (Show k, NamedResource k, Monad m) => k -> Based m URI
resourceURL k = do
    baseuri <- ask
    return $ resourceRelativeURLString k `relative` baseuri

-- | The URL for a specific version of a 'NamedResource' object.
resourceVersionURL :: (Show k, NamedResource k, Monad m)
                      => VersionedID k -> Based m URI
resourceVersionURL verk = do
    baseuri <- ask
    return $ (resourceRelativeURLString (thingid verk)
                ++ (show $ txnid $ verk))
             `relative` baseuri

-- | Augment JSON representations of 'Versioned' objects
-- with @"url"@ and @"thisVersion"@ fields.
instance (Show k, NamedResource k, ToJSON v)
         => BasedToJSON (Versioned k v) where
    basedToJSON v = do
        objurl <- resourceURL $ thingid $ version v
        verurl <- resourceVersionURL $ version v
        let origmap = case toJSON (thing v) of
                Object m -> m
                _ -> error "data did not encode as JSON object"
            augmentedmap = foldl' (flip (uncurry M.insert)) origmap
                [ "url" .= show objurl
                , "thisVersion" .= show verurl
                ]
        return $ Object augmentedmap

-- | Reporting results from 'get' and other getting methods.
instance (BasedToJSON a) => WebResult (GetResult a) where
    report (Right (Right a)) =
        do status ok200  -- http://tools.ietf.org/html/rfc2616#section-10.2.1
           json a
    report (Left NotFound) =
        status notFound404  -- http://tools.ietf.org/html/rfc2616#section-10.4.5
    report (Right (Left msg)) =
        do status internalServerError500  -- http://tools.ietf.org/html/rfc2616#section-10.5.1
           text msg

-- | Reporting results from 'update'.
instance (BasedToJSON b, NamedResource k, Show k, ToJSON v)
         => WebResult (UpdateResult b (Versioned k v)) where
    report (Right (Right (Right a))) =
        do status created201  -- http://tools.ietf.org/html/rfc2616#section-10.2.2
           url <- resourceVersionURL $ version a
           setHeader "Location" $ LT.pack $ show $ url
           json a
    report (Left (NewerVersion b)) =
        do status conflict409  -- http://tools.ietf.org/html/rfc2616#section-10.4.10
           json b
    report (Right gr) = report gr

-- | Reporting results from 'create'.
instance (NamedResource k, Show k, ToJSON v)
         => WebResult (CreateResult (Versioned k v)) where
    report (Right (Right (Right a))) =
        do status created201  -- http://tools.ietf.org/html/rfc2616#section-10.2.2
           url <- resourceURL $ thingid $ version a
           setHeader "Location" $ LT.pack $ show $ url
           json a
    report (Left (InvalidObject msgs)) =
        do status badRequest400
           text $ LT.concat [ LT.concat [msg, "\r\n"] | msg<-msgs ]
    report (Right gr) = report gr

lock :: MVar a -> (b -> IO c) -> b -> IO c
lock mv f x = withMVar mv $ const (f x)

instance (IsString k, Eq k, Parsable a, FromJSON a)
         => Default (Holex k ParamValue (VersionedID a)) where
    def = VersionedID <$> parse "id" <*> parse "txnid"

instance (IsString k, Eq k) => Default (Holex k ParamValue YoudoID) where
    def = parse "id"

instance (IsString k, Eq k) => Default (Holex k ParamValue YoudoData) where
    def = YoudoData <$> parse "assignerid"
                    <*> parse "assigneeid"
                    <*> defaultTo "" (parse "description")
                    <*> defaultTo (DueDate Nothing) (parse "duedate")
                    <*> defaultTo False (parse "completed")

instance (IsString k, Eq k)
         => Default (Holex k ParamValue (Versioned YoudoID YoudoUpdate)) where
    def = Versioned <$> (VersionedID <$> parse "id"
                                     <*> parse "txnid")
                    <*> (YoudoUpdate <$> optional (parse "assignerid")
                                     <*> optional (parse "assigneeid")
                                     <*> optional (parse "description")
                                     <*> optional (parse "duedate")
                                     <*> optional (parse "completed"))

instance (IsString k, Eq k) => Default (Holex k ParamValue UserID) where
    def = parse "id"

instance (IsString k, Eq k) => Default (Holex k ParamValue UserData) where
    def = UserData <$> parse "name"

instance (IsString k, Eq k) => Default (Holex k ParamValue (Versioned UserID UserUpdate)) where
    def = Versioned <$> (VersionedID <$> parse "id"
                                      <*> parse "txnid")
                    <*> (UserUpdate <$> optional (parse "name"))
