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
    -- * Interpreting requests
    RequestParser,
    -- * Reporting results
    WebResult(..)
) where

import Codec.MIME.Type (mimeType, MIMEType(Application))
import Codec.MIME.Parse (parseMIMEType)
import Control.Applicative
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad.Error (mapErrorT, throwError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (toJSON, ToJSON(..), Value(..), (.=))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import Data.Default
import Data.List (foldl', intercalate)
import Data.String (IsString(..))
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (ok200, created201, badRequest400, notFound404,
    methodNotAllowed405, internalServerError500, Status, StdMethod(..))
import Network.URI (URI(..), relativeTo, nullURI)
import Web.Scotty (ScottyM, matchAny, status, header,
    addroute, RoutePattern, params, text, json, setHeader,
    ActionM, raise, Parsable(..), body)
import Web.Scotty.Internal.Types (ActionT(..), ActionError(..),
    ScottyError(..))

import YouDo.Holex
import YouDo.Types

-- | The Scotty application.
-- Consists of 'webdb' interfaces for the given Youdo and User DB instances.
app :: ( DB YoudoID YoudoData YoudoUpdate IO ydb
       , DB UserID UserData UserUpdate IO udb
       ) => URI         -- ^The base URI of the app (without version number);
                        -- should end with a slash.
       -> ydb           -- ^A 'DB' instance for 'Youdo' objects.
       -> udb           -- ^A 'DB' instance for 'User' objects.
       -> MVar ()       -- ^All database access is under this MVar.
       -> ScottyM ()
app baseuri ydb udb mv = do
    let apibase = "./0/" `relative` baseuri
    webdb apibase mv ydb
    webdb apibase mv udb

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
         , Default (RequestParser u)
         ) => URI       -- ^The base URI of the API (including version number);
                        -- should end with a slash.
         -> MVar ()     -- ^All database access is under this MVar.
         -> d           -- ^The database.
         -> ScottyM()
webdb baseuri mv db =
    let basepath = nullURI { uriPath = uriPath baseuri }
        rtype = dbResourceName db Nothing
        pat s = fromString $ show $ s `relative` basepath
        onweb f = webfunc baseuri $ lock mv $ flip f db
    in do resource (pat rtype)
            [ (GET, onweb (\() -> getAll))
            , (POST, onweb post)
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
           => URI           -- ^The base URI of the app.
           -> (a -> IO r)   -- ^The function to perform.
           -> ActionM ()
webfunc u f =
    statusErrors $ do
        a <- failWith badRequest400 $ fromRequest $ def
        failWith internalServerError500 $ do
            r <- liftIO $ f a
            report u r

-- | A value that can be reported to a web client.
class WebResult r where
    report :: URI               -- ^The base URI of the app.
              -> r              -- ^The value to report.
              -> ActionM ()     -- ^An action that reports that value.

-- | Reporting versioned database objects as JSON.
instance (NamedResource k, Show k, ToJSON v)
         => WebResult (Versioned k v) where
    report baseuri x = json $ WebVersioned baseuri x

-- | Reporting lists of versioned database objects as JSON.
instance (NamedResource k, Show k, ToJSON v)
         => WebResult [Versioned k v] where
    report baseuri xs = json $ map (WebVersioned baseuri) xs

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

-- | Reporting results from 'get' and other "getting" methods.
instance (WebResult a) => WebResult (GetResult a) where
    report baseuri (Right (Right a)) =
        do status ok200
           report baseuri a
    report _ (Left NotFound) =
        status notFound404
    report _ (Right (Left msg)) =
        do status internalServerError500
           text msg

-- | Reporting results from 'update'.
instance (WebResult b, NamedResource k, Show k, ToJSON v)
         => WebResult (UpdateResult b (Versioned k v)) where
    report baseuri (Right (Right (Right a))) =
        do status created201
           setHeader "Location" $ LT.pack $ show $ resourceVersionURL baseuri (version a)
           report baseuri a
    report baseuri (Left (NewerVersion b)) =
        do status badRequest400
           report baseuri b
    report baseuri (Right gr) = report baseuri gr

-- | Reporting results from 'post'.
instance (NamedResource k, Show k, ToJSON v)
         => WebResult (PostResult (Versioned k v)) where
    report baseuri (Right (Right (Right a))) =
        do status created201
           setHeader "Location" $ LT.pack $ show $ resourceURL baseuri $ thingid $ version a
           report baseuri a
    report _ (Left (InvalidObject msgs)) =
        do status badRequest400
           text $ LT.concat [ LT.concat [msg, "\r\n"] | msg<-msgs ]
    report baseuri (Right gr) = report baseuri gr

-- | A web resource, with a complete list of its supported methods.
-- Defining a resource this way causes a 405 (Method Not Allowed)
-- responses when a request uses a method which is not in the
-- given list.  (Scotty's default is 404 (Not Found), which is less
-- appropriate.)
resource :: RoutePattern                    -- ^Route to this resource.
            -> [(StdMethod, ActionM ())]    -- ^Allowed methods and their actions.
            -> ScottyM ()
resource route acts =
    let allowedMethods = intercalate "," $ map (show . fst) acts
    in do
        sequence_ [addroute method route act | (method, act) <- acts]
        matchAny route $ do
            status methodNotAllowed405
            setHeader "Allow" $ LT.pack allowedMethods

-- | A 'Holex' for parsing data from HTTP requests.
type RequestParser = Holex LT.Text ParamValue

-- | Perform the given action, annotating any failures with the given
-- HTTP status.
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
statusErrors :: ActionT ErrorWithStatus IO () -> ActionM ()
statusErrors = (`bindError` reportStatus)
    where reportStatus (ErrorWithStatus stat msg) =
                do status stat
                   text msg

data ErrorWithStatus = ErrorWithStatus Status LT.Text
instance ScottyError ErrorWithStatus where
    stringError msg = ErrorWithStatus internalServerError500 (LT.pack msg)
    showError (ErrorWithStatus _ msg) = msg

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
fromRequest :: Holex LT.Text ParamValue a -> ActionM a
fromRequest expr = do
    kvs <- requestData
    case runHolex expr kvs of
        Left errs -> raise $ showHolexErrors errs
        Right a -> return a

-- | Get HTTP request data as key-value pairs, including
-- captures, query parameters, form data (in a request body of type
-- @application/x-www-form-url-encoded@), and values of a JSON object
-- (in a request body of type @application/json@).
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
