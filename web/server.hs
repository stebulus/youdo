{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (bracket)
import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Database.PostgreSQL.Simple (Connection, close)
import Network.Wai.Handler.Warp (Port, Settings(..), setPort, setHost,
    defaultSettings)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import System.Exit (exitWith, ExitCode(..))
import System.IO (stderr, hPutStrLn)
import System.Environment (getArgs, getProgName)
import Web.Scotty (scottyOpts, ScottyM, get, html, Options(..))

main = do
    progname <- getProgName
    args <- getArgs
    case parseArgs progname args of
        Left err -> do
            hPutStrLn stderr err
            exitWith $ ExitFailure 2
        Right (RunServer port pgurl) -> do
            withPostgresConnection pgurl $ \conn -> do
                mv_conn <- newMVar conn
                scottyOpts def{ verbose = 0
                              , settings = setPort port
                                         $ setHost "127.0.0.1"  -- for now
                                         $ defaultSettings
                              }
                           $ app mv_conn

withPostgresConnection :: URL -> (Connection -> IO a) -> IO a
withPostgresConnection pgurl f =
    bracket (connectPostgreSQL $ pack pgurl)
            (close)
            f

app :: MVar Connection -> ScottyM ()
app mv_conn = do
    get "/" $ html "placeholder"

data Action = RunServer Port URL
type URL = String

parseArgs :: String -> [String] -> Either String Action
parseArgs _ [port, pgurl] = Right $ RunServer (read port) pgurl
parseArgs progname _ = Left $ "usage: " ++ progname ++ " port postgresURL"
