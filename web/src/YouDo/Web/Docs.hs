{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : YouDo.Web.Docs
Description : Automatic documentation of web services.
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.Web.Docs (
    docs,
    HasJSONDescr(..),
    HasCaptureDescr(..))
where

import Control.Applicative (
    Const(..),
    (<*))
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Writer (tell, runWriter)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (StdMethod(GET))
import Network.URI (URI(..))
import Web.Scotty (text)

import YouDo.Const
import YouDo.Monad.Null
import YouDo.Web.ActionM
import YouDo.Web.Request
import YouDo.Web.Service

-- | Convert an 'API' to documentation.
docs :: API (APIDoc (NullMonad b)) -> API (ActionStatusM ())
docs api = resource [(GET, const $ lift500 $ text txt)]
    where txt = snd $ runWriter $ do
                    forM_ (toAssocList api) $ \(uri, acts) -> do
                        tell $ LT.replicate 40 "-"
                        tell "\r\n\r\n"
                        forM_ acts $ \(method, doc) -> do
                            tell $ LT.pack $ show method
                            tell " "
                            tell $ LT.pack $ uriPath uri
                            tell "\r\n"
                            tell $ getConst doc
                            tell "\r\n"

-- | The 'Applicative' that does the analysis work of 'docs'.
type APIDoc = Const LT.Text

instance FromRequestContext (ReaderT URI APIDoc) APIDoc where
    capture k = ReaderT $ const
        $ flip addConst ")\r\n"
        $ addCaptureDescr
        $ Const $ LT.concat [ "in url      : ", k, " (" ]
    body = ReaderT $ const $ template
instance FromRequestBodyContext APIDoc where
    parse k = flip addConst ")\r\n"
        $ addJSONDescr
        $ Const $ LT.concat [ "in json body: ", k, " (" ]
    defaultTo doc x = doc <* Const (LT.concat [
        "     default: ", LT.pack $ show x, "\r\n"
        ])
