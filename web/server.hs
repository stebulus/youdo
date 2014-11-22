{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding(id)
import Codec.MIME.Type (mimeType, MIMEType(Application))
import Codec.MIME.Parse (parseMIMEType)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Data.Maybe (listToMaybe)
import qualified Data.Text.Lazy as T
import Data.Time (UTCTime)
import Data.Time.Format (parseTime)
import Database.PostgreSQL.Simple (Connection, close)
import Network.BSD (getHostName)
import Network.HTTP.Types (created201, badRequest400)
import Network.URI (URI(..), URIAuth(..), relativeTo, nullURI)
import Network.Wai.Handler.Warp (Port, Settings(..), setPort, setHost,
    defaultSettings)
import Database.PostgreSQL.Simple (connectPostgreSQL, query, query_, execute,
    withTransaction, Only(..), Query)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import System.Exit (exitWith, ExitCode(..))
import System.IO (stderr, hPutStrLn)
import System.Environment (getArgs, getProgName)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import Web.Scotty (scottyOpts, ScottyM, get, post, status, header, param,
    text, Options(..), setHeader, ActionM, raise, rescue)

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
                let baseuri = nullURI { uriScheme = "http"
                                      , uriAuthority = Just URIAuth
                                            { uriUserInfo = ""
                                            , uriRegName = "localhost"
                                            , uriPort = if port == 80
                                                        then ""
                                                        else ":" ++ show port
                                            }
                                      , uriPath = "/"
                                      }
                scottyOpts def{ verbose = 0
                              , settings = setPort port
                                         $ setHost "127.0.0.1"  -- for now
                                         $ defaultSettings
                              }
                           $ app baseuri mv_conn

withPostgresConnection :: URL -> (Connection -> IO a) -> IO a
withPostgresConnection pgurl f =
    bracket (connectPostgreSQL $ pack pgurl)
            (close)
            f

app :: URI -> MVar Connection -> ScottyM ()
app baseuri mv_conn = do
    get "/" $ text "placeholder"
    get "/0/youdos" $ do
        youdos <- liftIO $ withMVar mv_conn getYoudos
        text $ T.pack $ show youdos
    post "/0/youdos" $ do
        youdo <- bodyYoudo
        youdoid <- liftIO $ withMVar mv_conn $ postYoudo youdo
        hostname <- header "Host" >>= maybe (T.pack <$> liftIO getHostName) return
        let url = T.pack $ show $
                nullURI { uriPath = "0/youdos/" ++ (show youdoid) }
                `relativeTo` baseuri
        status created201
        setHeader "Location" url
        text $ T.concat ["created at ", url, "\r\n"]

bodyYoudo :: ActionM Youdo
bodyYoudo = do
    hdr <- header "Content-Type"
        >>= maybe (raise "no Content-Type header") return
    contenttype <- maybe (raise $ T.concat ["Incomprehensible Content-Type: ", hdr])
                         return
                         (parseMIMEType $ T.toStrict hdr)
    case mimeType contenttype of
        Application "x-www-form-urlencoded" -> do
            assignerid <- param "assignerid"
            assigneeid <- param "assigneeid"
            description <- param "description"
            duedate <- fromISODateString <$> param "duedate"
            completed <- param "completed"
            return Youdo { id = Nothing
                         , assignerid = assignerid
                         , assigneeid = assigneeid
                         , description = description
                         , duedate = duedate
                         , completed = completed
                         }
        _ -> raise $ T.concat ["Don't know how to handle Content-Type: ", hdr]

postYoudo :: Youdo -> Connection -> IO Int
postYoudo youdo conn = do
    withTransaction conn $ do
        execute conn
                ("insert into transaction (yd_userid, yd_ipaddr, yd_useragent) \
                \values (?, ?, ?)"::Query)
                (0::Int, "127.0.0.1"::String, "some agent"::String)
        ids <- query conn
            "insert into youdo \
            \(assignerid, assigneeid, description, duedate, completed) \
            \values (?, ?, ?, ?, ?) returning id"
            (assignerid youdo, assigneeid youdo, description youdo,
            duedate youdo, completed youdo)
            :: IO [Only Int]
        return $ fromOnly $ head ids

data Youdo = Youdo { id :: Maybe Int  -- Nothing for new Youdos
                   , assignerid :: Int
                   , assigneeid :: Int
                   , description :: String
                   , duedate :: Maybe UTCTime
                   , completed :: Bool
                   } deriving (Show)
instance FromRow Youdo where
    fromRow = Youdo <$> field <*> field <*> field <*> field <*> field <*> field

getYoudos :: Connection -> IO [Youdo]
getYoudos conn = query_ conn
    "select id, assignerid, assigneeid, description, duedate, completed \
    \from youdo"

fromISODateString :: String -> Maybe UTCTime
fromISODateString s =
    parseTime defaultTimeLocale
              (iso8601DateFormat $ Just "%H:%M:%SZ")
              s

data Action = RunServer Port URL
type URL = String

parseArgs :: String -> [String] -> Either String Action
parseArgs _ [port, pgurl] = Right $ RunServer (read port) pgurl
parseArgs progname _ = Left $ "usage: " ++ progname ++ " port postgresURL"
