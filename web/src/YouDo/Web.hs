{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts,
    FlexibleInstances #-}
{-|
Module      : YouDo.Web
Description : Web application for YouDo
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.Web (
    -- * Web application
    app, webdb, webfunc, resource,
    -- * Base URIs
    Based, at,
    -- * Interpreting requests
    RequestParser, parse, ParamValue(..),
    -- * Reporting results
    WebResult(..)
) where

import Codec.MIME.Type (mimeType, MIMEType(Application))
import Codec.MIME.Parse (parseMIMEType)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad.Error (mapErrorT, throwError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ReaderT(..), mapReaderT)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import Data.Default
import Data.List (foldl', intercalate)
import Data.String (IsString(..))
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (ok200, created201, badRequest400, notFound404,
    methodNotAllowed405, conflict409, unsupportedMediaType415,
    internalServerError500, Status, StdMethod(..))
import Network.URI (URI(..), relativeTo, nullURI)
import Web.Scotty (ScottyM, matchAny, status, header,
    addroute, RoutePattern, params, text, json, setHeader,
    ActionM, Parsable(..), body)
import Web.Scotty.Internal.Types (ActionT(..), ActionError(..),
    ScottyError(..))

import YouDo.DB
import YouDo.Holex
import YouDo.Types

-- | Monad transformer for managing a base URI.
type Based = ReaderT URI

-- | Run a @Based@ with the given base URI.
at :: Based m a -> URI -> m a
at bma u = runReaderT bma u

-- | The Scotty application.
-- Consists of 'webdb' interfaces for the given Youdo and User DB instances.
app :: ( DB YoudoID YoudoData YoudoUpdate IO ydb
       , DB UserID UserData UserUpdate IO udb
       ) => YoudoDatabase ydb udb     -- ^The database.
       -> MVar ()       -- ^All database access is under this MVar.
       -> Based ScottyM ()
app db mv =
    local ("./0/" `relative`) $ do
        webdb mv (youdos db)
        webdb mv (users db)

-- | A web interface to an instance of 'DB'.
-- The following endpoints are created, relative to the given base URI
-- (which should probably end with a slash):
--
-- @
--      GET objs                (list of all current objs)
--      POST objs               (create new obj)
--      GET objs\//id/             (current version of obj)
--      GET objs\//id/\/versions    (all versions of obj)
--      GET objs\//id/\//txnid/       (specified version of obj)
--      POST objs\//id/\//txnid/      (create new version of obj)
-- @
--
-- These correspond directly to the methods of 'DB'.  The name @objs@
-- is obtained from the instance 'NamedResource' @k@.  Requests that
-- return objects return them in JSON format, using the instances
-- 'Show' @k@ and 'ToJSON' @v@.  The @/id/@ parameter is interpreted
-- via the instance 'Parsable' @k@.  (The 'FromJSON' @k@ instance
-- would only be used if the id were passed in the JSON request
-- body, which it shouldn't be.)  The request body, when needed,
-- is interpreted via default 'RequestParser' for the appropriate type.
webdb :: ( NamedResource k, DB k v u IO d
         , Parsable k, A.FromJSON k
         , Show k, ToJSON v
         , Default (RequestParser k)
         , Default (RequestParser v)
         , Default (RequestParser (Versioned k u))
         ) => MVar ()     -- ^All database access is under this MVar.
         -> d           -- ^The database.
         -> Based ScottyM ()
webdb mv db = do
    baseuri <- ask
    let basepath = nullURI { uriPath = uriPath baseuri }
        rtype = dbResourceName db
        pat s = fromString $ show $ s `relative` basepath
        onweb f = webfunc $ lock mv $ flip f db
    resource (pat rtype)
        [ (GET, onweb (\() -> getAll))
        , (POST, onweb create)
        ]
    resource (pat (rtype ++ "/:id"))
        [ (GET, onweb get) ]
    resource (pat (rtype ++ "/:id/versions"))
        [ (GET, onweb getVersions) ]
    resource (pat (rtype ++ "/:id/:txnid"))
        [ (GET, onweb getVersion)
        , (POST, onweb update)
        ]

-- | A web interface to a function.
-- A value of type @a@ is obtained from the HTTP request using the
-- type's default 'RequestParser'; then the given function is used
-- to obtain a 'WebResult', which is sent to the client.  If an error
-- occurs parsing the request, a 400 (Bad Request) response is sent;
-- errors in later phases cause 500 (Internal Server Error).
webfunc :: (WebResult r, Default (RequestParser a))
           => (a -> IO r)   -- ^The function to perform.
           -> Based ActionM ()
webfunc f =
    mapReaderT statusErrors $ do
        a <- lift $ fromRequest $ def
        mapReaderT lift500 $ do
            r <- liftIO $ f a
            report r

-- | A value that can be reported to a web client.
class WebResult r where
    report :: r                     -- ^The value to report.
              -> Based ActionM ()   -- ^An action that reports that value.

-- | Reporting versioned database objects as JSON.
instance (NamedResource k, Show k, ToJSON v)
         => WebResult (Versioned k v) where
    report x = do
        baseuri <- ask
        lift $ json $ WebVersioned baseuri x

-- | Reporting lists of versioned database objects as JSON.
instance (NamedResource k, Show k, ToJSON v)
         => WebResult [Versioned k v] where
    report xs = do
        baseuri <- ask
        lift $ json $ map (WebVersioned baseuri) xs

-- | Shim which augments JSON representations of 'Versioned' objects
-- with @"url"@ and @"thisVersion"@ fields.
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

-- | Reporting results from 'get' and other getting methods.
instance (WebResult a) => WebResult (GetResult a) where
    report (Right (Right a)) =
        do lift $ status ok200  -- http://tools.ietf.org/html/rfc2616#section-10.2.1
           report a
    report (Left NotFound) =
        lift $ status notFound404  -- http://tools.ietf.org/html/rfc2616#section-10.4.5
    report (Right (Left msg)) =
        do lift $ status internalServerError500  -- http://tools.ietf.org/html/rfc2616#section-10.5.1
           lift $ text msg

-- | Reporting results from 'update'.
instance (WebResult b, NamedResource k, Show k, ToJSON v)
         => WebResult (UpdateResult b (Versioned k v)) where
    report (Right (Right (Right a))) =
        do lift $ status created201  -- http://tools.ietf.org/html/rfc2616#section-10.2.2
           baseuri <- ask
           lift $ setHeader "Location" $ LT.pack $ show $ resourceVersionURL baseuri (version a)
           report a
    report (Left (NewerVersion b)) =
        do lift $ status conflict409  -- http://tools.ietf.org/html/rfc2616#section-10.4.10
           report b
    report (Right gr) = report gr

-- | Reporting results from 'create'.
instance (NamedResource k, Show k, ToJSON v)
         => WebResult (CreateResult (Versioned k v)) where
    report (Right (Right (Right a))) =
        do lift $ status created201  -- http://tools.ietf.org/html/rfc2616#section-10.2.2
           baseuri <- ask
           lift $ setHeader "Location" $ LT.pack $ show $ resourceURL baseuri $ thingid $ version a
           report a
    report (Left (InvalidObject msgs)) =
        do lift $ status badRequest400
           lift $ text $ LT.concat [ LT.concat [msg, "\r\n"] | msg<-msgs ]
    report (Right gr) = report gr

-- | A web resource, with a complete list of its supported methods.
-- Defining a resource this way causes a 405 (Method Not Allowed)
-- responses when a request uses a method which is not in the
-- given list.  (Scotty's default is 404 (Not Found), which is less
-- appropriate.)
resource :: RoutePattern                    -- ^Route to this resource.
            -> [(StdMethod, Based ActionM ())]    -- ^Allowed methods and their actions.
            -> Based ScottyM ()
resource route acts =
    let allowedMethods = intercalate "," $ map (show . fst) acts
    in do
        sequence_ [ mapReaderT (addroute method route) act
                  | (method, act) <- acts ]
        mapReaderT (matchAny route) $ do
            lift $ status methodNotAllowed405  -- http://tools.ietf.org/html/rfc2616#section-10.4.6
            lift $ setHeader "Allow" $ LT.pack allowedMethods

-- | Perform the given action, annotating any failures with the given
-- HTTP status.
failWith :: Status -> ActionM a -> ActionStatusM a
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

-- | Perform the given action, catching any Scotty exception raised.
catchActionError :: (ScottyError e, Monad m)
    => ActionT e m a -> ActionT e' m (Either (ActionError e) a)
catchActionError act =
    ActionT $ mapErrorT
        (\mea -> do
            ea <- mea
            return $ Right ea)
        (runAM act)

-- | Monadically alter the exception of a Scotty state.
bindError :: (ScottyError e, ScottyError e', Monad m)
    => ActionT e m a -> (e -> ActionT e' m a) -> ActionT e' m a
bindError act f = do
    eith <- catchActionError act
    case eith of
        Right a -> return a
        Left (ActionError e) -> f e
        Left (Redirect msg) -> throwError (Redirect msg)
        Left Next -> throwError Next

-- | Report any error status to the web client.
statusErrors :: ActionStatusM () -> ActionM ()
statusErrors = (`bindError` reportStatus)
    where reportStatus (ErrorWithStatus stat msg) =
                do status stat
                   text msg

type ActionStatusM a = ActionT ErrorWithStatus IO a

data ErrorWithStatus = ErrorWithStatus Status LT.Text
instance ScottyError ErrorWithStatus where
    stringError msg = ErrorWithStatus internalServerError500 (LT.pack msg)
    showError (ErrorWithStatus _ msg) = msg

raiseStatus :: Status -> LT.Text -> ActionStatusM a
raiseStatus stat msg = throwError $ ActionError $ ErrorWithStatus stat msg

-- | <http://tools.ietf.org/html/rfc2616#section-10.4.1>
badRequest :: LT.Text -> ActionStatusM a
badRequest = raiseStatus badRequest400

-- | Equivalent to 'failWith' 'internalServerError500'.
-- (See <http://tools.ietf.org/html/rfc2616#section-10.5.1>.)
lift500 :: ActionM a -> ActionStatusM a
lift500 = failWith internalServerError500

-- | The relative URL for a 'NamedResource' object.
resourceRelativeURLString :: (Show k, NamedResource k) => k -> String
resourceRelativeURLString k = "./" ++ resourceName (Just k) ++ "/" ++ show k

-- | The URL for a 'NamedResource' object.
resourceURL :: (Show k, NamedResource k) => URI -> k -> URI
resourceURL baseuri k = resourceRelativeURLString k `relative` baseuri

-- | The URL for a specific version of a 'NamedResource' object.
resourceVersionURL :: (Show k, NamedResource k) => URI -> VersionedID k -> URI
resourceVersionURL baseuri verk =
    (resourceRelativeURLString (thingid verk) ++ "/" ++ (show $ txnid $ verk))
    `relative` baseuri

-- | Use the given 'Holex' to interpret the data in the HTTP request.
fromRequest :: Holex LT.Text ParamValue a -> ActionStatusM a
fromRequest expr = do
    kvs <- requestData
    case runHolex expr kvs of
        Left errs -> badRequest $ showHolexErrors errs
        Right a -> return a

-- | Get HTTP request data as key-value pairs, including
-- captures, query parameters, form data (in a request body of type
-- @application/x-www-form-url-encoded@), and values of a JSON object
-- (in a request body of type @application/json@).
-- Raises HTTP status 415 (Unsupported Media Type) for other media types.
-- (See <http://tools.ietf.org/html/rfc2616#section-10.4.16>.)
requestData :: ActionStatusM [(LT.Text, ParamValue)]
requestData = do
    ps <- lift500 params
    let paramdata = [(k, ScottyParam v) | (k,v)<-ps]
    bodydata <- do
        maybehdr <- lift500 $ Web.Scotty.header "Content-Type"
        case maybehdr of
            Nothing -> return []
            Just hdr -> do
                let contenttype = parseMIMEType $ LT.toStrict hdr
                case mimeType <$> contenttype of
                    Just (Application "x-www-form-urlencoded") ->
                        -- form data is already in params
                        return []
                    Just (Application "json") -> do
                        bod <- lift500 body
                        case A.eitherDecode' bod of
                            Left err ->
                                badRequest $ LT.pack err
                            Right (Object obj) ->
                                return [(LT.fromStrict k, JSONField v) | (k,v)<-M.toList obj]
                            Right _ ->
                                badRequest "json payload is not an object"
                    Nothing -> badRequest $
                        LT.concat ["Incomprehensible Content-Type: ", hdr]
                    _ -> raiseStatus unsupportedMediaType415 $
                        LT.concat ["Don't know how to handle Content-Type: ", hdr]
    return $ paramdata ++ bodydata

-- | English description of a 'HolexError'.
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

-- | English description of a list of 'HolexError's.
showHolexErrors :: (Show k) => [HolexError k v] -> LT.Text
showHolexErrors es = LT.concat [ LT.concat [ showHolexError e, "\r\n" ]
                               | e<-es ]

-- | Dereference a relative URI path.  Usually used infix.
relative :: String      -- ^The path.
            -> URI      -- ^The base URI.
            -> URI
relative s u = nullURI { uriPath = s } `relativeTo` u

lock :: MVar a -> (b -> IO c) -> b -> IO c
lock mv f x = withMVar mv $ const (f x)

-- | A 'Holex' for parsing data from HTTP requests.
type RequestParser = Holex LT.Text ParamValue

instance (IsString k, Eq k, Parsable a, FromJSON a)
         => Default (Holex k ParamValue (VersionedID a)) where
    def = VersionedID <$> parse "id" <*> parse "txnid"

instance (IsString k, Eq k) => Default (Holex k ParamValue YoudoID) where
    def = parse "id"

instance (IsString k, Eq k) => Default (Holex k ParamValue YoudoData) where
    def = YoudoData <$> parse "assignerid"
                    <*> parse "assigneeid"
                    <*> defaultTo "" (parse "description")
                    <*> defaultTo (DueDate Nothing) (parse "duedate")
                    <*> defaultTo False (parse "completed")

instance (IsString k, Eq k)
         => Default (Holex k ParamValue (Versioned YoudoID YoudoUpdate)) where
    def = Versioned <$> (VersionedID <$> parse "id"
                                     <*> parse "txnid")
                    <*> (YoudoUpdate <$> optional (parse "assignerid")
                                     <*> optional (parse "assigneeid")
                                     <*> optional (parse "description")
                                     <*> optional (parse "duedate")
                                     <*> optional (parse "completed"))

instance (IsString k, Eq k) => Default (Holex k ParamValue ()) where
    def = Const ()

parse :: (Eq k, Parsable a, FromJSON a) => k -> Holex k ParamValue a
parse k = tryApply
    (Const (\x ->
        case x of
            ScottyParam txt ->
                case parseParam txt of
                    Left err -> Left (ParseError k x err)
                    Right val -> Right val
            JSONField jsonval ->
                case parseEither parseJSON jsonval of
                    Left err -> Left (ParseError k x (LT.pack err))
                    Right val -> Right val))
    $ hole k

data ParamValue = ScottyParam LT.Text
                | JSONField Value
    deriving (Eq, Show)

instance (IsString k, Eq k) => Default (Holex k ParamValue UserID) where
    def = parse "id"

instance (IsString k, Eq k) => Default (Holex k ParamValue UserData) where
    def = UserData <$> parse "name"

instance (IsString k, Eq k) => Default (Holex k ParamValue (Versioned UserID UserUpdate)) where
    def = Versioned <$> (VersionedID <$> parse "id"
                                      <*> parse "txnid")
                    <*> (UserUpdate <$> optional (parse "name"))
