{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module YouDo.Web where
import Codec.MIME.Type (mimeType, MIMEType(Application))
import Codec.MIME.Parse (parseMIMEType)
import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket)
import Control.Monad.Error (mapErrorT, throwError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (toJSON, ToJSON(..), Value(..), (.=))
import Data.Aeson.Types (parseEither)
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
    methodNotAllowed405, internalServerError500, Status, StdMethod(..))
import Network.URI (URI(..), URIAuth(..), relativeTo, nullURI)
import Network.Wai.Handler.Warp (setPort, setHost, defaultSettings)
import Options.Applicative (option, strOption, flag', auto, long, short,
    metavar, help, execParser, Parser, fullDesc, helper, info)
import qualified Options.Applicative as Opt
import Web.Scotty (scottyOpts, ScottyM, get, matchAny, status, header,
    addroute, RoutePattern, params, text, json, Options(..), setHeader,
    ActionM, raise, Parsable(..), body)
import Web.Scotty.Internal.Types (ActionT(..), ActionError(..),
    ScottyError(..))
import YouDo.DB
import qualified YouDo.DB.Mock as Mock
import YouDo.DB.PostgreSQL(DBConnection(..))
import YouDo.Holex (runHolex, hole, optional, defaultTo, Holex(..),
    HolexError(..), tryApply)

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
        ),(POST, statusErrors $ do
            yd <- failWith badRequest400 $ fromRequest $
                    YoudoData <$> parse "assignerid"
                              <*> parse "assigneeid"
                              <*> defaultTo "" (parse "description")
                              <*> defaultTo (DueDate Nothing) (parse "duedate")
                              <*> defaultTo False (parse "completed")
            failWith internalServerError500 $ do
                ydid <- liftIO $ withMVar mv_db $ postYoudo yd
                let url = LT.pack $ youdoURL baseuri ydid
                status created201
                setHeader "Location" url
                text $ LT.concat ["created at ", url, "\r\n"]
        )]
    resource "/0/youdos/:id"
        [(GET, statusErrors $ do
            ydid <- failWith badRequest400 $ fromRequest $ YoudoID <$> parse "id"
            failWith internalServerError500 $ do
                yds <- liftIO $ withMVar mv_db $ getYoudo ydid
                case yds of
                    [] -> do status notFound404
                             text $ LT.concat ["no youdo with id ", LT.pack $ show ydid]
                    [yd] -> do status ok200
                               json (WebYoudo baseuri yd)
                    _ -> do raise $ LT.concat ["multiple youdos with id ", LT.pack $ show ydid]
        )]
    resource "/0/youdos/:id/versions"
        [(GET, statusErrors $ do
            ydid <- failWith badRequest400 $ fromRequest $ YoudoID <$> parse "id"
            failWith internalServerError500 $ do
                ydvers <- liftIO $ withMVar mv_db $ getYoudoVersions ydid
                status ok200
                json $ map (WebYoudo baseuri) ydvers
        )]
    resource "/0/youdos/:id/:txnid"
        [(GET, statusErrors $ do
            ydver <- failWith badRequest400 $ fromRequest $
                YoudoVersionID <$> parse "id" <*> parse "txnid"
            failWith internalServerError500 $ do
                youdos <- liftIO $ withMVar mv_db $ getYoudoVersion ydver
                case youdos of
                    [] -> do status notFound404
                             text $ LT.concat ["no youdo with ", LT.pack $ show ydver]
                    [yd] -> do status ok200
                               json (WebYoudo baseuri yd)
                    _ -> raise $ LT.concat ["multiple youdos with ", LT.pack $ show ydver]
        ),(POST, statusErrors $ do
            ydupd <- failWith badRequest400 $ fromRequest $
                YoudoUpdate <$> (YoudoVersionID <$> parse "id"
                                                <*> parse "txnid")
                            <*> optional (parse "assignerid")
                            <*> optional (parse "assigneeid")
                            <*> optional (parse "description")
                            <*> optional (parse "duedate")
                            <*> optional (parse "completed")
            failWith internalServerError500 $ do
                result <- liftIO $ withMVar mv_db $ updateYoudo ydupd
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

-- Perform the given action, annotating any failures with the given status.
failWith :: Status -> ActionM a -> ActionT ErrorWithStatus IO a
failWith stat act =
    ActionT $ mapErrorT
        (\m -> do
            eith <- m
            return $ case eith of
                Left (ActionError msg) ->
                    Left $ ActionError $ ErrorWithStatus stat msg
                Left Next -> Left Next
                Left (Redirect msg) -> Left $ Redirect msg
                Right x -> Right x)
        (runAM act)

catchActionError :: (ScottyError e, Monad m)
    => ActionT e m a -> ActionT e' m (Either (ActionError e) a)
catchActionError act =
    ActionT $ mapErrorT
        (\mea -> do
            ea <- mea
            return $ Right ea)
        (runAM act)

withFail :: (ScottyError e, ScottyError e', Monad m)
    => (e -> ActionT e' m a) -> ActionT e m a -> ActionT e' m a
withFail f act = do
    eith <- catchActionError act
    case eith of
        Right a -> return a
        Left (ActionError e) -> f e
        Left (Redirect msg) -> throwError (Redirect msg)
        Left Next -> throwError Next

statusErrors :: ActionT ErrorWithStatus IO () -> ActionM ()
statusErrors = withFail report
    where report (ErrorWithStatus stat msg) =
                do status stat
                   text msg

data ErrorWithStatus = ErrorWithStatus Status LT.Text
instance ScottyError ErrorWithStatus where
    stringError msg = ErrorWithStatus internalServerError500 (LT.pack msg)
    showError (ErrorWithStatus _ msg) = msg

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

fromRequest :: Holex LT.Text ParamValue a -> ActionM a
fromRequest expr = do
    kvs <- requestData
    case runHolex expr kvs of
        Left errs -> raise $ showHolexErrors errs
        Right a -> return a

requestData :: ActionM [(LT.Text, ParamValue)]
requestData = do
    ps <- params
    let paramdata = [(k, ScottyParam v) | (k,v)<-ps]
    bodydata <- do
        maybehdr <- Web.Scotty.header "Content-Type"
        case maybehdr of
            Nothing -> return []
            Just hdr -> do
                let contenttype = parseMIMEType $ LT.toStrict hdr
                case mimeType <$> contenttype of
                    Just (Application "x-www-form-urlencoded") ->
                        -- form data is already in params
                        return []
                    Just (Application "json") -> do
                        bod <- body
                        case A.eitherDecode' bod of
                            Left err ->
                                raise (LT.pack err)
                            Right (Object obj) ->
                                return [(LT.fromStrict k, JSONField v) | (k,v)<-M.toList obj]
                            Right _ ->
                                raise "json payload is not an object"
                    Nothing -> raise $ LT.concat ["Incomprehensible Content-Type: ", hdr]
                    _ -> raise $ LT.concat ["Don't know how to handle Content-Type: ", hdr]
    return $ paramdata ++ bodydata

parse :: (Eq k, Parsable a, A.FromJSON a) => k -> Holex k ParamValue a
parse k = tryApply
    (Const (\x ->
        case x of
            ScottyParam txt ->
                case parseParam txt of
                    Left err -> Left (ParseError k x err)
                    Right val -> Right val
            JSONField jsonval ->
                case parseEither A.parseJSON jsonval of
                    Left err -> Left (ParseError k x (LT.pack err))
                    Right val -> Right val))
    $ hole k

data ParamValue = ScottyParam LT.Text
                | JSONField Value
    deriving (Eq, Show)

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
