{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Default (def)
import Network.Wai.Handler.Warp (Port, Settings(..), setPort, setHost,
    defaultSettings)
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
        Right (RunServer port pgurl) ->
            scottyOpts def{ verbose = 0
                          , settings = setPort port
                                     $ setHost "127.0.0.1"  -- for now
                                     $ defaultSettings
                          }
                       app

app :: ScottyM ()
app = do
    get "/" $ html "placeholder"

data Action = RunServer Port URL
type URL = String

parseArgs :: String -> [String] -> Either String Action
parseArgs _ [port, pgurl] = Right $ RunServer (read port) pgurl
parseArgs progname _ = Left $ "usage: " ++ progname ++ " port postgresURL"
