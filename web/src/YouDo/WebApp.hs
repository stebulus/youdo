{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts,
    FlexibleInstances #-}
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
import Data.ByteString.Char8 (pack)
import Data.Default
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Network.URI (URI(..), URIAuth(..), nullURI)
import Network.Wai.Handler.Warp (setPort, setHost, defaultSettings)
import Options.Applicative (option, strOption, flag', auto, long, short,
    metavar, help, execParser, Parser, fullDesc, helper, info, header)
import Web.Scotty (scottyOpts, Options(..), ScottyM)

import YouDo.DB
import YouDo.DB.Memory
import YouDo.DB.PostgreSQL
import YouDo.Web
import YouDo.Types

-- | The Scotty application.
-- Consists of 'webdb' interfaces for the given Youdo and User DB instances.
app :: ( DB IO YoudoID YoudoData YoudoUpdate ydb
       , DB IO UserID UserData UserUpdate udb
       ) => YoudoDatabase IO ydb udb     -- ^The database.
       -> URI           -- ^The base URI.
       -> ScottyM ()
app db uri = toScotty $ fmap join
                      $ (fmap.fmap) ($ mapYoudoDB liftIO db)
                      $ uri // api0

-- | The Youdo API, version 0.
api0 :: ( DB ActionStatusM YoudoID YoudoData YoudoUpdate ydb
        , DB ActionStatusM UserID UserData UserUpdate udb
        )
     => API (ActionStatusM (YoudoDatabase ActionStatusM ydb udb -> ActionStatusM ()))
api0 =
    u"0" // ( setBase $
        (pullback youdos webdb)
        <>
        (pullback users webdb)
    )
    where pullback f api = (fmap.fmap) (. f) api

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
