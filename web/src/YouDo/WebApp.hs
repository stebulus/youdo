{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts,
    FlexibleInstances, UndecidableInstances #-}
{-|
Module      : YouDo.WebApp
Description : Web application for YouDo
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.WebApp (app, webdb) where

import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad.Reader.Class (MonadReader(..))
import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as A
import Data.Default
import Network.HTTP.Types (StdMethod(..))
import Web.Scotty (ScottyM, Parsable(..))

import YouDo.DB
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

lock :: MVar a -> (b -> IO c) -> b -> IO c
lock mv f x = withMVar mv $ const (f x)
