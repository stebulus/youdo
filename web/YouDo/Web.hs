{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module YouDo.Web where
import Prelude hiding(id)
import Codec.MIME.Type (mimeType, MIMEType(Application))
import Codec.MIME.Parse (parseMIMEType)
import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, toJSON, Value(..))
import Data.ByteString.Char8 (pack)
import qualified Data.HashMap.Strict as M
import Data.Default (def)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import Data.Time (UTCTime)
import Data.Time.Format (parseTime)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Network.HTTP.Types (ok200, created201, notFound404,
    internalServerError500)
import Network.URI (URI(..), URIAuth(..), relativeTo, nullURI)
import Network.Wai.Handler.Warp (setPort, setHost, defaultSettings)
import Options.Applicative (option, strOption, flag', auto, long, short,
    metavar, help, execParser, Parser, fullDesc, helper, info)
import qualified Options.Applicative as Opt
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import Web.Scotty (scottyOpts, ScottyM, get, post, status, header, param,
    text, Options(..), setHeader, ActionM, raise, raw)
import YouDo.DB
import qualified YouDo.DB.Mock as Mock
import YouDo.DB.PostgreSQL(DBConnection(..))

data DBOption = InMemory | Postgres String
data YDOptions = YDOptions { port :: Int
                           , db :: DBOption
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
            (fullDesc <> Opt.header "ydserver - a YouDo web server")

mainOpts :: YDOptions -> IO ()
mainOpts YDOptions { port = p, db = dbopt } =
    withDB dbopt $ \db' -> do
        mvdb <- newMVar db'
        let baseuri = nullURI { uriScheme = "http"
                              , uriAuthority = Just URIAuth
                                    { uriUserInfo = ""
                                    , uriRegName = "localhost"
                                    , uriPort = if p == 80
                                                then ""
                                                else ":" ++ show p
                                    }
                              , uriPath = "/"
                              }
        scottyOpts def{ verbose = 0
                      , settings = setPort p
                                 $ setHost "127.0.0.1"  -- for now
                                 $ defaultSettings
                      }
                   $ app baseuri mvdb

withDB :: DBOption -> (forall a. DB a => a -> IO b) -> IO b
withDB InMemory f = Mock.empty >>= f
withDB (Postgres connstr) f =
    bracket (DBConnection <$> connectPostgreSQL (pack connstr))
            (\(DBConnection conn) -> close conn)
            f

app :: DB a => URI -> MVar a -> ScottyM ()
app baseuri mv_db = do
    get "/" $ text "placeholder"
    get "/0/youdos" $ do
        youdos <- liftIO $ withMVar mv_db getYoudos
        text $ LT.pack $ show youdos
    post "/0/youdos" $ do
        youdo <- bodyYoudo
        youdoid <- liftIO $ withMVar mv_db $ postYoudo youdo
        let url = LT.pack $ youdoURL baseuri youdoid
        status created201
        setHeader "Location" url
        text $ LT.concat ["created at ", url, "\r\n"]
    get "/0/youdos/:id" $ do
        ydid <- read <$> param "id" :: ActionM Int
        youdos <- liftIO $ withMVar mv_db $ getYoudo ydid
        case youdos of
            [] -> do status notFound404
                     text $ LT.concat ["no youdo with id ", LT.pack $ show ydid]
            [youdo] -> do status ok200
                          setHeader "Content-Type" "application/json"
                          let (Object obj) = toJSON youdo
                              augmented = Object $ M.insert "url"
                                (String (ST.pack (youdoURL baseuri ydid)))
                                obj
                          raw $ encode augmented
            _ -> do status internalServerError500
                    text $ LT.concat ["multiple youdos with id ", LT.pack $ show ydid]

youdoURL :: URI -> Int -> String
youdoURL baseuri ydid = show $
    nullURI { uriPath = "0/youdos/" ++ (show ydid) }
    `relativeTo` baseuri

bodyYoudo :: ActionM Youdo
bodyYoudo = do
    hdr <- Web.Scotty.header "Content-Type"
        >>= maybe (raise "no Content-Type header") return
    contenttype <- maybe (raise $ LT.concat ["Incomprehensible Content-Type: ", hdr])
                         return
                         (parseMIMEType $ LT.toStrict hdr)
    case mimeType contenttype of
        Application "x-www-form-urlencoded" -> do
            assignerid' <- param "assignerid"
            assigneeid' <- param "assigneeid"
            description' <- param "description"
            duedate' <- fromISODateString <$> param "duedate"
            completed' <- param "completed"
            return Youdo { id = Nothing
                         , assignerid = assignerid'
                         , assigneeid = assigneeid'
                         , description = description'
                         , duedate = duedate'
                         , completed = completed'
                         }
        _ -> raise $ LT.concat ["Don't know how to handle Content-Type: ", hdr]

fromISODateString :: String -> Maybe UTCTime
fromISODateString s =
    parseTime defaultTimeLocale
              (iso8601DateFormat $ Just "%H:%M:%SZ")
              s
