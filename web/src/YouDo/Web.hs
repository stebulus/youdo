{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts,
    FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-|
Module      : YouDo.Web
Description : A tiny web framework on top of Scotty.
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.Web (
    -- * Resources as bundles of operations
    resource,
    -- * Base URIs
    BasedToJSON(..), BasedFromJSON(..), BasedParsable(..), basedjson,
    -- * Interpreting requests
    capture, FromParam(..),
    body, fromRequestBody, RequestParser, parse, ParamValue(..), requestData,
    EvaluationError(..), defaultTo, optional,
    Constructor, Constructible(..),
    -- * Reporting results
    WebResult(..),
    -- * Error handling and HTTP status
    ActionStatusM, ErrorWithStatus, raiseStatus, failWith,
    catchActionError, bindError, statusErrors, badRequest, lift500
) where

import Codec.MIME.Type (mimeType, MIMEType(Application))
import Codec.MIME.Parse (parseMIMEType)
import Control.Applicative ((<$>), Applicative(..))
import Control.Monad.Error (mapErrorT, throwError)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (MonadTrans(..))
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as M
import Data.List (intercalate)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (badRequest400, methodNotAllowed405,
    unsupportedMediaType415, internalServerError500, Status, StdMethod(..))
import Network.URI (URI(..))
import Web.Scotty (ScottyM, matchAny, header, addroute, param, params,
    ActionM, Parsable(..), status, setHeader, RoutePattern)
import qualified Web.Scotty as Scotty
import Web.Scotty.Internal.Types (ActionT(..), ActionError(..),
    ScottyError(..))

import YouDo.Holes

-- | A web resource, with a complete list of its supported methods.
-- Defining a resource this way causes a 405 (Method Not Allowed)
-- response when a request uses a method which is not in the
-- given list.  (Scotty's default is 404 (Not Found), which is less
-- appropriate.)
resource :: RoutePattern            -- ^Route to this resource.
            -> [(StdMethod, ActionStatusM ())]
                                    -- ^Allowed methods and their actions.
            -> ScottyM ()
resource route acts =
    let allowedMethods = intercalate "," $ map (show . fst) acts
    in do
        sequence_ [ addroute method route $ statusErrors act
                  | (method, act) <- acts ]
        matchAny route $ do
            status methodNotAllowed405  -- http://tools.ietf.org/html/rfc2616#section-10.4.6
            setHeader "Allow" $ LT.pack allowedMethods

{- |
    Get the value of a Scotty capture.

    Due to limitations of Scotty, this could in theory retrieve the
    value of a field in the form data or a query parameter, but only
    if there's no capture with the given name.  In practice this is
    not a problem because @capture@ is normally used right next to
    the route pattern that declares the relevant captures, so there's
    little room for error.  (See 'YouDo.DB.webdb', for example.)

    The returned action is wrapped in a 'MonadTrans' for compatibility
    with other combinators used in request-parsing applicative
    expressions, such as 'body', which produce values of type 'ReaderT
    URI ActionStatusM b' to depend on the base URI.  (This one does
    not actually depend on the base URI.)

    If the capture is not found, @capture@ raises the status HTTP 500
    (Internal Server Error), because you shouldn't ask for captures
    that you don't know are there.  If the capture cannot be parsed
    as type @a@ then 'Scotty.next' is called (as with 'param'); if
    no other route pattern matches the request (the usual situation)
    then this will result in an HTTP 404 (Not Found), which makes
    sense because presumably an URL that doesn't parse according to
    the server's expectations doesn't denote any existing resource.
-}
capture :: (FromParam a b, MonadTrans t)
        => LT.Text                          -- ^The name of the capture.
        -> t ActionStatusM b      -- ^The action to get its value.
capture k = lift $ lift500 $ fromParam <$> param k

-- | How to convert a Scotty capture to type b.
-- There is no provision for failure; report failures in the
-- methods of the underlying 'Parsable' instance.
-- (This typeclass exists for parsing captures, as distinct
-- from parsing form data payload; it will disappear when
-- BasedParsable is used by 'body'.)
class (Parsable a) => FromParam a b | b->a where
    fromParam :: a->b

-- | Like 'Scotty.json', but for based representations.
basedjson :: BasedToJSON a => a -> URI -> ActionM ()
basedjson x uri = Scotty.json $ basedToJSON x uri

-- | A value that can be reported to a web client.
class WebResult r where
    report :: r                           -- ^The value to report.
              -> URI                      -- ^The base URI.
              -> ActionStatusM ()         -- ^An action that reports that value.

-- | A value that can be serialized as JSON, respecting a base URI.
class BasedToJSON a where
    basedToJSON :: a -> URI -> Value
instance BasedToJSON a => BasedToJSON [a] where
    basedToJSON xs base = toJSON $ map (flip basedToJSON base) xs

-- | A value that can be deserialized from JSON, respecting a base URI.
class BasedFromJSON a where
    basedParseJSON :: Value -> URI -> A.Parser a

-- | A value that can be deserialized from a Scotty param, respecting a base URI.
class BasedParsable a where
    basedParseParam :: LT.Text -> URI -> Either LT.Text a

type ActionStatusM = ActionT ErrorWithStatus IO

data ErrorWithStatus = ErrorWithStatus Status LT.Text
instance ScottyError ErrorWithStatus where
    stringError msg = ErrorWithStatus internalServerError500 (LT.pack msg)
    showError (ErrorWithStatus _ msg) = msg

raiseStatus :: Status -> LT.Text -> ActionStatusM a
raiseStatus stat msg = throwError $ ActionError $ ErrorWithStatus stat msg

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
                do Scotty.status stat
                   Scotty.text msg

-- | <http://tools.ietf.org/html/rfc2616#section-10.4.1>
badRequest :: LT.Text -> ActionStatusM a
badRequest = raiseStatus badRequest400

-- | Equivalent to 'failWith' 'internalServerError500'.
-- (See <http://tools.ietf.org/html/rfc2616#section-10.5.1>.)
lift500 :: ActionM a -> ActionStatusM a
lift500 = failWith internalServerError500

{- |
    A value of type @a@ is obtained from the HTTP request using the
    'construct' method of the 'Constructible' instance; usually you
    will have defined an instance like

    @
        instance ('Constructor' f) => 'Constructible' (f MyType) where
            construct = MyType \<$\> parse \"id\" \<*\> parse \"name\"
    @

    because then your construction can be used with any suitable
    functor @f@.  (For use with @body@, we only need the instance
    for 'RequestParser', but you might as well write it generically.)

    If an error occurs parsing the request, a 400 (Bad Request)
    response is thrown.
-}
body :: (Constructible (RequestParser a)) => ReaderT URI ActionStatusM a
body = do
    base <- ask
    lift $ fromRequestBody construct base

{- |
    Use the given 'RequestParser' to interpret the data in the HTTP
    request.  See the discussion in 'body' for the usual method
    of defining a 'RequestParser'.

    If an error occurs parsing the request, a 400 (Bad Request)
    response is thrown.
-}
fromRequestBody :: RequestParser a -> URI -> ActionStatusM a
fromRequestBody expr _ = do
    kvs <- requestData
    case evaluateE expr kvs of
        Left errs -> badRequest $ showEvaluationErrors errs
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
                        bod <- lift500 Scotty.body
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

{- |
    An applicative functor that can construct objects from
    HTTP request data.
-}
class ( Applicative f
      , Holes LT.Text ParamValue f
      , Errs [EvaluationError LT.Text ParamValue] f
      )
     => Constructor f

instance Constructor RequestParser

-- | An object that can be constructed by a 'Constructor'.
-- Typically the 'construct' method should be implemented as an
-- applicative expression; see the example under 'body'.
class Constructible a where
    construct :: a

-- | Unit can be constructed (as a 'pure' value).
instance (Applicative f) => Constructible (f ()) where
    construct = pure ()

-- | An error encountered when constructing an object by filling in
-- values for its 'hole's and calling 'evaluateE'.
data EvaluationError k v
    = MissingKey k             -- ^A hole named @k@ was not filled.
    | UnusedKey k              -- ^A value for a hole named @k@ was given,
                               -- but there is no such hole.
    | DuplicateValue k v       -- ^Value @v@ was given for a hole named @k@,
                               -- after another value was already given.
    | ParseError k v LT.Text   -- ^An error was detected by 'parse'.
    | CheckError LT.Text       -- ^An error was detected by 'check'.
    deriving (Show, Eq)

-- | If the first argument fails to evaluate due to a single
-- 'MissingKey' error, replace it with the second argument.
-- Other error situations are left as is.
-- (Combinator for use in 'construct' implementations.)
defaultTo :: (Applicative f, Errs [EvaluationError k v] f)
          => f a -> a -> f a
defaultTo fa d = fa `catch` \es ->
    case es of
        [MissingKey _] -> pure d
        _ -> throw $ pure es

-- | If the argument fails to evaluate due to a single 'MissingKey'
-- error, replace it with 'Nothing'; otherwise, 'Just' the first
-- argument.
optional :: (Applicative f, Errs [EvaluationError k v] f)
         => f a -> f (Maybe a)
optional fa = (Just <$> fa) `defaultTo` Nothing

-- | English description of an 'EvaluationError'.
showEvaluationError :: (Show k) => EvaluationError k v -> LT.Text
showEvaluationError (MissingKey k) = LT.concat [ "missing mandatory parameter "
                                               , LT.pack (show k)
                                               ]
showEvaluationError (UnusedKey k) = LT.concat [ "unknown parameter "
                                              , LT.pack (show k)
                                              ]
showEvaluationError (DuplicateValue k _) = LT.concat [ "duplicate value for parameter "
                                                     , LT.pack (show k)
                                                     ]
showEvaluationError (ParseError k _ msg) = LT.concat [ "cannot parse parameter "
                                                     , LT.pack (show k)
                                                     , ": "
                                                     , msg
                                                     ]
showEvaluationError (CheckError e) = LT.pack (show e)

-- | English description of a list of 'EvaluationError's.
showEvaluationErrors :: (Show k) => [EvaluationError k v] -> LT.Text
showEvaluationErrors es = LT.concat [ LT.concat [ showEvaluationError e, "\r\n" ]
                                    | e<-es ]

-- | Type synonym for our usual evaluation applicative.
type RequestParser = EvaluatorE LT.Text ParamValue [EvaluationError LT.Text ParamValue]

-- | How to report missing keys in a 'RequestParser'.
instance MissingKeyError LT.Text [EvaluationError LT.Text ParamValue] where
    missingKeyError k = [MissingKey k]

-- | A named hole which parses a 'ParamValue' into the appropriate type.
-- (Combinator for use in 'construct' implementations.)
parse :: ( Functor f
         , Parsable a, FromJSON a
         , Holes k ParamValue f
         , Errs [EvaluationError k ParamValue] f
         )
      => k -> f a
parse k = throwLeft $ enlist <$> (parseEither k <$> hole k)
    where enlist (Left e) = Left [e]
          enlist (Right a) = Right a

-- | Parse a 'ParamValue' into the appropriate type.
parseEither :: (Parsable a, FromJSON a)
            => k -> ParamValue -> Either (EvaluationError k ParamValue) a
parseEither k x@(ScottyParam txt) =
    case parseParam txt of
        Left err -> Left (ParseError k x err)
        Right val -> Right val
parseEither k x@(JSONField jsonval) =
    case A.parseEither parseJSON jsonval of
        Left err -> Left (ParseError k x (LT.pack err))
        Right val -> Right val

-- | The values obtainable from an HTTP request.
data ParamValue = ScottyParam LT.Text
                | JSONField Value
    deriving (Eq, Show)
