{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts,
    FlexibleInstances, MultiParamTypeClasses #-}
{-|
Module      : YouDo.WebApp
Description : Web application for YouDo
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3

YouDo web server and its command-line interface.
-}
module YouDo.WebApp (
    app, main, mainOpts, YDOptions(..), DBOption(..), options
) where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.ByteString.Char8 (pack)
import Data.Default
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Network.HTTP.Types (StdMethod(GET))
import Network.URI (URI(..), URIAuth(..), nullURI)
import Network.Wai.Handler.Warp (setPort, setHost, defaultSettings)
import Options.Applicative (option, strOption, flag', auto, long, short,
    metavar, help, execParser, Parser, fullDesc, helper, info, header)
import Web.Scotty (scottyOpts, Options(..), ScottyM)

import YouDo.DB
import YouDo.DB.Memory
import YouDo.DB.PostgreSQL
import YouDo.Monad.Null
import YouDo.Web
import YouDo.Types

-- | The Scotty application.
-- Consists of 'webdb' interfaces for the given Youdo and User DB instances.
app :: ( DB IO YoudoID YoudoData YoudoUpdate ydb
       , DB IO UserID UserData UserUpdate udb
       , TxnDB IO tdb
       )
    => YoudoDatabase IO ydb udb tdb     -- ^The database.
    -> URI                              -- ^The base URI.
    -> ScottyM ()
app db uri = toScotty $
                fmap join (uri // api0 (mapYoudoDB liftIO db))
                <> uri // u"apidocs" //
                    docs (uri // api0 (YoudoDatabase NullYoudoDB NullUserDB NullTxnDB))

data NullYoudoDB = NullYoudoDB
instance DB NullMonad YoudoID YoudoData YoudoUpdate NullYoudoDB where
    get = const $ const NullMonad
    getVersion = const $ const NullMonad
    getVersions = const $ const NullMonad
    getAll = const NullMonad
    create = const $ const NullMonad
    update = const $ const NullMonad
data NullUserDB = NullUserDB
instance DB NullMonad UserID UserData UserUpdate NullUserDB where
    get = const $ const NullMonad
    getVersion = const $ const NullMonad
    getVersions = const $ const NullMonad
    getAll = const NullMonad
    create = const $ const NullMonad
    update = const $ const NullMonad
data NullTxnDB = NullTxnDB
instance TxnDB NullMonad NullTxnDB where
    getTxn = const $ const NullMonad

-- | The Youdo API, version 0.
api0 :: ( DB m YoudoID YoudoData YoudoUpdate ydb
        , DB m UserID UserData UserUpdate udb
        , TxnDB m tdb
        , FromRequestBody g YoudoData
        , FromRequestBody g YoudoUpdate
        , FromRequestBody g UserData
        , FromRequestBody g UserUpdate
        , FromRequestContext (ReaderT URI f) g
        , WebResult m (GetResult Youdo)
        , WebResult m (GetResult [Youdo])
        , WebResult m (CreateResult Youdo)
        , WebResult m (UpdateResult Youdo Youdo)
        , WebResult m (GetResult User)
        , WebResult m (GetResult [User])
        , WebResult m (CreateResult User)
        , WebResult m (UpdateResult User User)
        , WebResult m (GetResult TxnDeluxe)
        , Applicative f
        )
     => YoudoDatabase m ydb udb tdb
     -> API (f (m ()))
api0 db =
    u"0" // ( setBase $
        ((fmap.fmap) ($ youdos db) webdb)
        <>
        ((fmap.fmap) ($ users db) webdb)
        <>
        ((fmap.fmap) ($ db) webtxndb)
    )

webtxndb :: ( DB m YoudoID YoudoData YoudoUpdate y
            , DB m UserID UserData UserUpdate u
            , TxnDB m t
            , WebResult m (GetResult TxnDeluxe)
            , FromRequestContext (ReaderT URI f) g
            , Applicative f
            )
         => API (f (YoudoDatabase m y u t -> m ()))
webtxndb =
    u (resourceName (Nothing :: Maybe TransactionID)) // u":txnid" //
        resource [ (GET, dodb $ getTxnDeluxe <$> capture "txnid") ]
    where dodb rdrt uri = (fmap.fmap) (>>= report uri) (runReaderT rdrt uri)

-- | The kind of database to connect to.
data DBOption = InMemory            -- ^A transient in-memory database; see "YouDo.DB.Memory"
              | Postgres String     -- ^A PostgreSQL database; see "YouDo.DB.PostgreSQL"

data YDOptions = YDOptions { port :: Int        -- ^What port to listen to.
                           , dbopt :: DBOption  -- ^What kind of database to connect to.
                           }

options :: Parser YDOptions
options = YDOptions
    <$> option auto (long "port"
                     <> short 'p'
                     <> metavar "PORT"
                     <> help "Listen on port number PORT.")
    <*> ( Postgres <$> strOption (long "postgresql"
                                 <> short 'g'
                                 <> metavar "STR"
                                 <> help "Connect to PostgreSQL database \
                                         \specified by libpq connection \
                                         \string STR.")
          <|> flag' InMemory (long "memory"
                             <> short 'm'
                             <> help "Use a transient in-memory database.") )

main :: IO ()
main = execParser opts >>= mainOpts
    where opts = info (helper <*> options)
            (fullDesc <> header "ydserver - a YouDo web server")

mainOpts :: YDOptions -> IO ()
mainOpts opts = do
    let baseuri = nullURI { uriScheme = "http:"
                          , uriAuthority = Just URIAuth
                                { uriUserInfo = ""
                                , uriRegName = "localhost"
                                , uriPort = if p == 80
                                            then ""
                                            else ":" ++ show p
                                }
                          , uriPath = "/"
                          }
        scotty = scottyOpts def{ verbose = 0
                               , settings = setPort p
                                          $ setHost "127.0.0.1"  -- for now
                                          $ defaultSettings
                               }
        runApp db = scotty $ app db baseuri
        p = port opts
    case dbopt opts of
        InMemory -> YouDo.DB.Memory.empty >>= runApp
        Postgres connstr -> do
            bracket (connectPostgreSQL (pack connstr))
                    (\conn -> close conn)
                    (\conn -> youdoDB conn >>= runApp)
