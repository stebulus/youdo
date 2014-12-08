{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module YouDo.Web where
import Prelude hiding(id)
import Codec.MIME.Type (mimeType, MIMEType(Application))
import Codec.MIME.Parse (parseMIMEType)
import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..), left, right, hoistEither)
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
    addroute, RoutePattern, param, params, text, json, Options(..), setHeader,
    ActionM, Parsable(..), raise)
import YouDo.DB
import qualified YouDo.DB.Mock as Mock
import YouDo.DB.PostgreSQL(DBConnection(..))
import YouDo.Holex

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
                yd <- bodyYoudoData
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
        ydid <- YoudoID <$> read <$> param "id"
        youdos <- liftIO $ withMVar mv_db $ getYoudo ydid
        case youdos of
            [] -> do status notFound404
                     text $ LT.concat ["no youdo with id ", LT.pack $ show ydid]
            [yd] -> do status ok200
                       json (WebYoudo baseuri yd)
            _ -> do status internalServerError500
                    text $ LT.concat ["multiple youdos with id ", LT.pack $ show ydid]
        )]
    resource "/0/youdos/:id/versions" [(GET, do
        ydid' <- runEitherT bodyYoudoID
        case ydid' of
            Left err -> do status badRequest400
                           text err
            Right ydid -> do
                ydvers <- liftIO $ withMVar mv_db $ getYoudoVersions ydid
                status ok200
                json $ map (WebYoudoVersionID baseuri) ydvers
        )]
    resource "/0/youdos/:id/:txnid" [(GET, do
        ydver <- runEitherT bodyYoudoVersionID
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
            ydupd <- runEitherT bodyYoudoUpdate
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

bodyYoudoUpdate :: EitherT LT.Text ActionM YoudoUpdate
bodyYoudoUpdate = do
    hdr <- Web.Scotty.header "Content-Type"
        `maybeError` "no Content-Type header"
    contenttype <- (return $ parseMIMEType $ LT.toStrict hdr)
        `maybeError` (LT.concat ["Incomprehensible Content-Type: ", hdr])
    case mimeType contenttype of
        Application "x-www-form-urlencoded" -> do
            id <- mandatoryParam "id"
            txnid <- mandatoryParam "txnid"
            assignerid' <- maybeParam "assignerid"
            assigneeid' <- maybeParam "assigneeid"
            description' <- maybeParam "description"
            duedate' <- maybeParam "duedate"
            completed' <- maybeParam "completed"
            right YoudoUpdate { oldVersion = YoudoVersionID id txnid
                              , newAssignerid = assignerid'
                              , newAssigneeid = assigneeid'
                              , newDescription = description'
                              , newDuedate = duedate'
                              , newCompleted = completed'
                              }
        _ -> left $ LT.concat ["Don't know how to handle Content-Type: ", hdr]

bodyYoudoData :: EitherT LT.Text ActionM YoudoData
bodyYoudoData = do
    hdr <- Web.Scotty.header "Content-Type"
        `maybeError` "no Content-Type header"
    contenttype <- (return $ parseMIMEType $ LT.toStrict hdr)
        `maybeError` (LT.concat ["Incomprehensible Content-Type: ", hdr])
    case mimeType contenttype of
        Application "x-www-form-urlencoded" -> do
            assignerid' <- mandatoryParam "assignerid"
            assigneeid' <- mandatoryParam "assigneeid"
            description' <- optionalParam "description" ""
            duedate' <- optionalParam "duedate" (DueDate Nothing)
            completed' <- optionalParam "completed" False
            right YoudoData { assignerid = assignerid'
                            , assigneeid = assigneeid'
                            , description = description'
                            , duedate = duedate'
                            , completed = completed'
                            }
        _ -> left $ LT.concat ["Don't know how to handle Content-Type: ", hdr]

bodyYoudoID :: EitherT LT.Text ActionM YoudoID
bodyYoudoID = do
    ydid <- mandatoryParam "id"
    right $ YoudoID ydid

bodyYoudoVersionID :: EitherT LT.Text ActionM YoudoVersionID
bodyYoudoVersionID = do
    ydid <- mandatoryParam "id"
    txnid <- mandatoryParam "txnid"
    right YoudoVersionID { youdoid = ydid, youdotxnid = txnid }

showHolexError :: (Show k, Show e) => HolexError k v e -> LT.Text
showHolexError (MissingKey k) = LT.concat [ "missing mandatory parameter "
                                          , LT.pack (show k)
                                          ]
showHolexError (UnusedKey k) = LT.concat [ "unknown parameter "
                                         , LT.pack (show k)
                                         ]
showHolexError (DuplicateValue k _) = LT.concat [ "duplicate value for parameter "
                                                , LT.pack (show k)
                                                ]
showHolexError (CustomError e) = LT.pack (show e)

showHolexErrors :: (Show k, Show e) => [HolexError k v e] -> LT.Text
showHolexErrors es = LT.concat [ LT.concat [ showHolexError e, "\r\n" ]
                               | e<-es ]

maybeError :: (Monad m) => m (Maybe a) -> b -> EitherT b m a
maybeError mma err = do
    ma <- lift mma
    case ma of
        Nothing -> left err
        Just x -> right x

mandatoryParam :: (Parsable a) => LT.Text -> EitherT LT.Text ActionM a
mandatoryParam key = do
    ps <- lift params
    case lookup key ps of
        Nothing -> left $ LT.concat ["missing mandatory parameter ", key]
        Just val -> hoistEither $ parseParam val

optionalParam :: (Parsable a) => LT.Text -> a -> EitherT LT.Text ActionM a
optionalParam key defaultval = do
    ps <- lift params
    case lookup key ps of
        Nothing -> right defaultval
        Just val -> hoistEither $ parseParam val

maybeParam :: (Parsable a) => LT.Text -> EitherT LT.Text ActionM (Maybe a)
maybeParam key = do
    ps <- lift params
    case lookup key ps of
        Nothing -> right Nothing
        Just val -> hoistEither $ Just <$> parseParam val
