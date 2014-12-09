{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module YouDo.Web where
import Codec.MIME.Type (mimeType, MIMEType(Application))
import Codec.MIME.Parse (parseMIMEType)
import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..), left, right, hoistEither,
    bimapEitherT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (toJSON, ToJSON(..), Value(..), (.=))
import qualified Data.Aeson as A
import Data.ByteString.Char8 (pack)
import qualified Data.HashMap.Strict as M
import Data.Default (def)
import Data.List (foldl', intercalate)
import Data.Monoid ((<>))
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Network.HTTP.Types (ok200, created201, badRequest400, notFound404,
    methodNotAllowed405, internalServerError500, StdMethod(..))
import Network.URI (URI(..), URIAuth(..), relativeTo, nullURI)
import Network.Wai.Handler.Warp (setPort, setHost, defaultSettings)
import Options.Applicative (option, strOption, flag', auto, long, short,
    metavar, help, execParser, Parser, fullDesc, helper, info)
import qualified Options.Applicative as Opt
import Web.Scotty (scottyOpts, ScottyM, get, matchAny, status, header,
    addroute, RoutePattern, params, text, json, Options(..), setHeader,
    ActionM, raise)
import YouDo.DB
import qualified YouDo.DB.Mock as Mock
import YouDo.DB.PostgreSQL(DBConnection(..))
import YouDo.Holex (runHolex, parse, optional, defaultTo, Holex(..),
    HolexError(..))

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
    resource "/0/youdos"
        [(GET, do
            youdos <- liftIO $ withMVar mv_db getYoudos
            text $ LT.pack $ show youdos
        ),(POST, do
            err_url <- runEitherT $ do
                yd <- fromParams $
                    YoudoData <$> parse "assignerid"
                              <*> parse "assigneeid"
                              <*> defaultTo "" (parse "description")
                              <*> defaultTo (DueDate Nothing) (parse "duedate")
                              <*> defaultTo False (parse "completed")
                ydid <- liftIO $ withMVar mv_db $ postYoudo yd
                return $ LT.pack $ youdoURL baseuri ydid
            case err_url of
                Left err -> do status badRequest400
                               text err
                Right url -> do status created201
                                setHeader "Location" url
                                text $ LT.concat ["created at ", url, "\r\n"]
        )]
    resource "/0/youdos/:id" [(GET, do
        err_ydid <- runEitherT $ fromParams $ YoudoID <$> parse "id"
        case err_ydid of
            Left err -> do status badRequest400
                           text err
            Right ydid -> do
                yds <- liftIO $ withMVar mv_db $ getYoudo ydid
                case yds of
                    [] -> do status notFound404
                             text $ LT.concat ["no youdo with id ", LT.pack $ show ydid]
                    [yd] -> do status ok200
                               json (WebYoudo baseuri yd)
                    _ -> do status internalServerError500
                            text $ LT.concat ["multiple youdos with id ", LT.pack $ show ydid]
        )]
    resource "/0/youdos/:id/versions" [(GET, do
        ydid' <- runEitherT $ bodyData $ YoudoID <$> parse "id"
        case ydid' of
            Left err -> do status badRequest400
                           text err
            Right ydid -> do
                ydvers <- liftIO $ withMVar mv_db $ getYoudoVersions ydid
                status ok200
                json $ map (WebYoudo baseuri) ydvers
        )]
    resource "/0/youdos/:id/:txnid" [(GET, do
        ydver <- runEitherT $ bodyData $ YoudoVersionID <$> parse "id" <*> parse "txnid"
        case ydver of
            Left err -> do status badRequest400
                           text err
            Right ver -> do
                youdos <- liftIO $ withMVar mv_db $ getYoudoVersion ver
                case youdos of
                    [] -> do status notFound404
                             text $ LT.concat ["no youdo with ", LT.pack $ show ver]
                    [yd] -> do status ok200
                               json (WebYoudo baseuri yd)
                    _ -> do status internalServerError500
                            text $ LT.concat ["multiple youdos with ", LT.pack $ show ver]
        ),(POST, do
            ydupd <- runEitherT $ fromParams $
                YoudoUpdate <$> (YoudoVersionID <$> parse "id"
                                                <*> parse "txnid")
                            <*> optional (parse "assignerid")
                            <*> optional (parse "assigneeid")
                            <*> optional (parse "description")
                            <*> optional (parse "duedate")
                            <*> optional (parse "completed")
            case ydupd of
                Left err -> do status badRequest400
                               text err
                Right upd -> do result <- liftIO $ withMVar mv_db $ updateYoudo upd
                                case result of
                                    OldVersion newver ->
                                        do status badRequest400
                                           text $ LT.concat
                                                [ "cannot modify old version; modify "
                                                , LT.pack $ youdoVersionURL baseuri newver
                                                ]
                                    Failure err -> raise $ LT.pack err
                                    Success ydver ->
                                        let url = LT.pack $ youdoVersionURL baseuri ydver
                                        in do status created201
                                              setHeader "Location" url
                                              text $ LT.concat ["created at ", url, "\r\n"]
        )]

resource :: RoutePattern -> [(StdMethod, ActionM ())] -> ScottyM ()
resource route acts =
    let allowedMethods = intercalate "," $ map (show . fst) acts
    in do
        sequence_ [addroute method route act | (method, act) <- acts]
        matchAny route $ do
            status methodNotAllowed405
            setHeader "Allow" $ LT.pack allowedMethods
            text ""

data WebYoudo = WebYoudo URI Youdo
instance ToJSON WebYoudo where
    toJSON (WebYoudo baseuri yd) = Object augmentedmap
        where augmentedmap = foldl' (flip (uncurry M.insert)) origmap
                    [ "url" .= youdoURL baseuri (youdoid (version yd))
                    , "thisVersion" .= youdoVersionURL baseuri (version yd)
                    ]
              origmap = case toJSON yd of
                            Object m -> m
                            _ -> error "Youdo didn't become a JSON object"

data WebYoudoVersionID = WebYoudoVersionID URI YoudoVersionID
instance ToJSON WebYoudoVersionID where
    toJSON (WebYoudoVersionID baseuri ydver) =
        A.String $ ST.pack $ youdoVersionURL baseuri ydver

youdoURL :: URI -> YoudoID -> String
youdoURL baseuri (YoudoID n) = show $
    nullURI { uriPath = "0/youdos/" ++ (show n) }
    `relativeTo` baseuri

youdoVersionURL :: URI -> YoudoVersionID -> String
youdoVersionURL baseuri (YoudoVersionID (YoudoID yd) (TransactionID txn))
    = show $
        nullURI { uriPath = "0/youdos/" ++ (show yd) ++ "/" ++ (show txn) }
        `relativeTo` baseuri

fromParams :: Holex LT.Text LT.Text a -> EitherT LT.Text ActionM a
fromParams expr = do
    maybehdr <- lift $ Web.Scotty.header "Content-Type"
    case maybehdr of
        Nothing -> bodyData expr
        Just hdr -> do
            contenttype <- (return $ parseMIMEType $ LT.toStrict hdr)
                `maybeError` (LT.concat ["Incomprehensible Content-Type: ", hdr])
            case mimeType contenttype of
                Application "x-www-form-urlencoded" -> bodyData expr
                _ -> left $ LT.concat ["Don't know how to handle Content-Type: ", hdr]

bodyData :: Holex LT.Text LT.Text a -> EitherT LT.Text ActionM a
bodyData holex = do
    ps <- lift params
    bimapEitherT showHolexErrors id $ hoistEither $ runHolex holex ps

showHolexError :: (Show k) => HolexError k v -> LT.Text
showHolexError (MissingKey k) = LT.concat [ "missing mandatory parameter "
                                          , LT.pack (show k)
                                          ]
showHolexError (UnusedKey k) = LT.concat [ "unknown parameter "
                                         , LT.pack (show k)
                                         ]
showHolexError (DuplicateValue k _) = LT.concat [ "duplicate value for parameter "
                                                , LT.pack (show k)
                                                ]
showHolexError (ParseError k _ msg) = LT.concat [ "cannot parse parameter "
                                                , LT.pack (show k)
                                                , ": "
                                                , msg
                                                ]
showHolexError (CustomError e) = LT.pack (show e)

showHolexErrors :: (Show k) => [HolexError k v] -> LT.Text
showHolexErrors es = LT.concat [ LT.concat [ showHolexError e, "\r\n" ]
                               | e<-es ]

maybeError :: (Monad m) => m (Maybe a) -> b -> EitherT b m a
maybeError mma err = do
    ma <- lift mma
    case ma of
        Nothing -> left err
        Just x -> right x
