{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : YouDo.Web.Main
Description : Command-line interface for YouDo web server
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3

Command-line interface for the web server defined in "YouDo.WebApp".
-}
module YouDo.Web.Main (main, mainOpts, YDOptions(..), DBOption(..)) where

import Control.Applicative
import Control.Concurrent.MVar (newMVar)
import Control.Exception (bracket)
import Data.ByteString.Char8 (pack)
import Data.Default
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Network.URI (URI(..), URIAuth(..), nullURI)
import Network.Wai.Handler.Warp (setPort, setHost, defaultSettings)
import Options.Applicative (option, strOption, flag', auto, long, short,
    metavar, help, execParser, Parser, fullDesc, helper, info, header)
import Web.Scotty (scottyOpts, Options(..))

import YouDo.DB.Memory
import YouDo.DB.PostgreSQL
import YouDo.Types (YoudoDatabase(..))
import YouDo.Web (at)
import YouDo.WebApp (app)

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
        runApp db mv = scotty $ app db mv `at` baseuri
        p = port opts
    mv <- newMVar ()
    case dbopt opts of
        InMemory -> do
            db <- YouDo.DB.Memory.empty
            runApp db mv
        Postgres connstr -> do
            bracket (connectPostgreSQL (pack connstr))
                    (\conn -> close conn)
                    (\conn -> runApp (YoudoDatabase
                                        (PostgresYoudoDB conn)
                                        (PostgresUserDB conn))
                                     mv)
