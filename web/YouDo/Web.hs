{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
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
import Data.Default
import Data.List (foldl', intercalate)
import Data.Monoid ((<>))
import Data.String (IsString(..))
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
import Web.Scotty (scottyOpts, ScottyM, matchAny, status, header,
    addroute, RoutePattern, params, text, json, Options(..), setHeader,
    ActionM, raise, Parsable(..), body)
import Web.Scotty.Internal.Types (ActionT(..), ActionError(..),
    ScottyError(..))
import YouDo.DB.Mock
import YouDo.DB.PostgreSQL
import YouDo.Holex (runHolex, hole, optional, defaultTo, Holex(..),
    HolexError(..), tryApply)
import YouDo.Types

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
mainOpts YDOptions { port = p, db = dbopt } = do
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
    mv <- newMVar ()
    case dbopt of
        InMemory -> do
            db <- empty
            scotty $ app baseuri (MockYoudoDB db) (MockUserDB db) db mv
        Postgres connstr -> do
            bracket (connectPostgreSQL (pack connstr))
                    (\conn -> close conn)
                    (\conn -> scotty $ app baseuri
                                           (PostgresYoudoDB conn)
                                           (PostgresUserDB conn)
                                           (PostgresDB conn)
                                           mv)

app :: ( DB YoudoID YoudoData YoudoUpdate IO ydb
       , DB UserID UserData UserUpdate IO udb
       , YoudoDB d
       ) => URI -> ydb -> udb -> d -> MVar () -> ScottyM ()
app baseuri ydb udb db mv = do
    let apibase = "./0/" `relative` baseuri
    resource "/0/youdos"
        [(GET, dbAction' mv db
            (Const ())
            (const getYoudos)
            (\yds -> do status ok200
                        text $ LT.pack $ show yds)
        ),(POST, dbAction mv ydb
            (YoudoData <$> parse "assignerid"
                       <*> parse "assigneeid"
                       <*> defaultTo "" (parse "description")
                       <*> defaultTo (DueDate Nothing) (parse "duedate")
                       <*> defaultTo False (parse "completed"))
            post
            (\ydid -> do
                let url = LT.pack $ show $ resourceURL apibase ydid
                status created201
                setHeader "Location" url
                text $ LT.concat ["created at ", url, "\r\n"])
        )]
    webdb apibase mv ydb
    webdb apibase mv udb

webdb :: ( NamedResource k, DB k v u IO d
         , Parsable k, A.FromJSON k
         , Show k, ToJSON v
         , Default (RequestParser u)
         ) => URI -> MVar () -> d -> ScottyM()
webdb baseuri mv db =
    let resourcebase = (dbResourceName db Nothing ++ "/") `relative` baseuri
        s = uriPath $ ":id" `relative` resourcebase
    in do resource (fromString s)
            [(GET, dbAction mv db
                (parse "id")
                get
                (\xs -> case xs of
                    [x] -> do status ok200
                              json (WebVersioned baseuri x)
                    [] -> status notFound404
                    _ -> do status internalServerError500
                            text "multiple objects found!")
            )]
          resource (fromString $ s ++ "/versions")
            [(GET, dbAction mv db
                (parse "id")
                getVersions
                (\xs -> do status ok200
                           json $ map (WebVersioned baseuri) xs)
            )]
          resource (fromString $ s ++ "/:txnid")
            [(GET, dbAction mv db
                (VersionedID <$> parse "id" <*> parse "txnid")
                getVersion
                (\xs -> case xs of
                    [x] -> do status ok200
                              json (WebVersioned baseuri x)
                    [] -> do status notFound404
                    _ -> raise "multiple objects found!")
            ),(POST, dbAction mv db
                def
                update
                (\result -> case result of
                    OldVersion newver ->
                        do status badRequest400
                           text $ LT.concat
                                [ "cannot modify old version; modify "
                                , LT.pack $ show $ resourceVersionURL baseuri newver
                                ]
                    Failure err -> raise $ LT.pack err
                    Success ydver ->
                        let url = LT.pack $ show $ resourceVersionURL baseuri ydver
                        in do status created201
                              setHeader "Location" url
                              text $ LT.concat ["created at ", url, "\r\n"])
            )]

resource :: RoutePattern -> [(StdMethod, ActionM ())] -> ScottyM ()
resource route acts =
    let allowedMethods = intercalate "," $ map (show . fst) acts
    in do
        sequence_ [addroute method route act | (method, act) <- acts]
        matchAny route $ do
            status methodNotAllowed405
            setHeader "Allow" $ LT.pack allowedMethods

type RequestParser = Holex LT.Text ParamValue

dbAction :: (DB k v u IO d)
    => MVar ()
    -> d
    -> RequestParser a
    -> (a -> d -> IO c)
    -> (c -> ActionM ())
    -> ActionM ()
dbAction mv db expr work resp =
    statusErrors $ do
        a <- failWith badRequest400 $ fromRequest $ expr
        c <- liftIO $ withMVar mv $ \_ -> work a db
        failWith internalServerError500 $ resp c
dbAction' :: (YoudoDB d)
    => MVar ()
    -> d
    -> RequestParser a
    -> (a -> d -> IO c)
    -> (c -> ActionM ())
    -> ActionM ()
dbAction' mv db expr work resp =
    statusErrors $ do
        a <- failWith badRequest400 $ fromRequest $ expr
        c <- liftIO $ withMVar mv $ \_ -> work a db
        failWith internalServerError500 $ resp c

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

bindError :: (ScottyError e, ScottyError e', Monad m)
    => ActionT e m a -> (e -> ActionT e' m a) -> ActionT e' m a
bindError act f = do
    eith <- catchActionError act
    case eith of
        Right a -> return a
        Left (ActionError e) -> f e
        Left (Redirect msg) -> throwError (Redirect msg)
        Left Next -> throwError Next

statusErrors :: ActionT ErrorWithStatus IO () -> ActionM ()
statusErrors = (`bindError` report)
    where report (ErrorWithStatus stat msg) =
                do status stat
                   text msg

data ErrorWithStatus = ErrorWithStatus Status LT.Text
instance ScottyError ErrorWithStatus where
    stringError msg = ErrorWithStatus internalServerError500 (LT.pack msg)
    showError (ErrorWithStatus _ msg) = msg

data WebVersioned k v = WebVersioned URI (Versioned k v)
instance (Show k, NamedResource k, ToJSON v) => ToJSON (WebVersioned k v) where
    toJSON (WebVersioned baseuri ver) = Object augmentedmap
        where augmentedmap = foldl' (flip (uncurry M.insert)) origmap
                    [ "url" .= show (objurl `relative` baseuri)
                    , "thisVersion" .= show (verurl `relative` baseuri)
                    ]
              verurl = objurl ++ "/" ++ (show verid)
              objurl = resourceName (Just $ thingid $ version ver) ++ "/" ++ (show vid)
              vid = thingid $ version ver
              verid = txnid $ version ver
              origmap = case toJSON (thing ver) of
                            Object m -> m
                            _ -> error "data did not encode as JSON object"

resourceRelativeURLString :: (Show k, NamedResource k) => k -> String
resourceRelativeURLString k = "./" ++ resourceName (Just k) ++ "/" ++ show k

resourceURL :: (Show k, NamedResource k) => URI -> k -> URI
resourceURL baseuri k = resourceRelativeURLString k `relative` baseuri

resourceVersionURL :: (Show k, NamedResource k) => URI -> VersionedID k -> URI
resourceVersionURL baseuri verk =
    (resourceRelativeURLString (thingid verk) ++ "/" ++ (show $ txnid $ verk))
    `relative` baseuri

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

relative :: String -> URI -> URI
relative s u = nullURI { uriPath = s } `relativeTo` u
