{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts,
    FlexibleInstances, UndecidableInstances #-}
{-|
Module      : YouDo.WebApp
Description : Web application for YouDo
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.WebApp (app, webdb) where

import Control.Concurrent.MVar
import Control.Monad.Reader.Class (local)
import Web.Scotty (ScottyM)

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
