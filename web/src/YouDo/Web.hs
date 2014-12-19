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
    resource, API(..), setBase, toScotty,
    -- * Base and relative URIs
    BasedToJSON(..), BasedFromJSON(..), BasedParsable(..), basedjson,
    Based(..), RelativeToURI(..), (//), u,
    -- * Interpreting requests
    capture,
    body, fromRequestBody, RequestParser, parse, ParamValue(..), requestData,
    EvaluationError(..), defaultTo, optional,
    RequestParsable(..),
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
import Control.Monad.Reader (ReaderT(..), ask, mapReaderT)
import Control.Monad.Trans (MonadTrans(..))
import Data.Aeson (ToJSON(..), Value(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as M
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (badRequest400, methodNotAllowed405,
    unsupportedMediaType415, internalServerError500, Status, StdMethod(..))
import Network.URI (URI(..), relativeTo, nullURI)
import Web.Scotty (ScottyM, matchAny, header, addroute, param, params,
    ActionM, Parsable(..), status, setHeader)
import qualified Web.Scotty as Scotty
import Web.Scotty.Internal.Types (ActionT(..), ActionError(..),
    ScottyError(..))

import YouDo.Holes

data Based a = Based { base :: Maybe URI
                     , relToBase :: URI
                     , payload :: a
                     }
instance RelativeToURI (Based a) where
    uri .// x@Based { base = Nothing } = x { relToBase = uri .// (relToBase x) }
    uri .// x@Based { base = Just y } = x { base = Just (uri .// y) }

newtype API = API [Based [(StdMethod, URI -> ActionStatusM ())]]
instance RelativeToURI API where
    uri .// (API xs) = API $ map (uri .//) xs
instance Monoid API where
    mempty = API mempty
    (API xs) `mappend` (API ys) = API (xs `mappend` ys)
setBase :: API -> API
setBase (API xs) = API $ map f xs
    where f x = x { base = Just $ maybe nullURI id (base x) }

toScotty :: API -> ScottyM ()
toScotty (API xs) = mapM_ f xs
    where f basable =
            let baseuri = fromMaybe nullURI $ base basable
                route = fromString $ uriPath (baseuri .// relToBase basable)
                allowedMethods = intercalate ","
                    $ map (show . fst) (payload basable)
            in do
                sequence_ [ addroute method route
                            $ statusErrors
                            $ actf baseuri
                          | (method, actf)<-payload basable ]
                matchAny route $ do
                    status methodNotAllowed405
                        -- http://tools.ietf.org/html/rfc2616#section-10.4.6
                    setHeader "Allow" $ LT.pack allowedMethods

-- | A web resource, with a complete list of its supported methods.
-- Defining a resource this way causes a 405 (Method Not Allowed)
-- response when a request uses a method which is not in the
-- given list.  (Scotty's default is 404 (Not Found), which is less
-- appropriate.)
resource :: [(StdMethod, URI -> ActionStatusM ())]
                                    -- ^Allowed methods and their actions.
            -> API
resource acts = API [Based { base = Nothing
                           , relToBase = nullURI
                           , payload = acts
                           }]

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
capture :: (Parsable a, MonadTrans t)
        => LT.Text                -- ^The name of the capture.
        -> t ActionStatusM a      -- ^The action to get its value.
capture k = lift $ lift500 $ param k

-- | Like 'Scotty.json', but for based representations.
basedjson :: BasedToJSON a => a -> URI -> ActionM ()
basedjson x uri = Scotty.json $ basedToJSON x uri

-- | Something that can be evaluated relative to a URI.
class RelativeToURI a where
    (.//) :: URI -> a -> a
instance RelativeToURI URI where
    a .// b = b `relativeTo` a
infixl 7 .//

{- |
    This version of '(.//)' ensures that there is a slash
    at the end of the path part of the URI before evaluating 'b'
    relative to it. Thus, for example,

    @
        u "foo" // u "bar" == u "foo/bar"
    @

    which is probably what the writer intended.
-}
(//) :: (RelativeToURI b) => URI -> b -> b
a // b = a' .// b
    where a' = if maybeLast apath == Just '/'
               then a
               else a { uriPath = apath ++ "/" }
          apath = uriPath a
          maybeLast [] = Nothing
          maybeLast xs = Just $ last xs
infixl 7 //

-- | Create a 'URI' with the given string as path component.
-- (It's probably best to use this only for string literals
-- whose validity you can see yourself.)
u :: String -> URI
u s = nullURI { uriPath = s }

-- | A value that can be reported to a web client.
class WebResult r where
    report :: r                     -- ^The value to report.
           -> URI                   -- ^The base URI.
           -> ActionStatusM ()      -- ^An action that reports that value.

-- | A value that can be serialized as JSON, respecting a base URI.
class BasedToJSON a where
    basedToJSON :: a -> URI -> Value
instance BasedToJSON a => BasedToJSON [a] where
    basedToJSON xs uri = toJSON $ map (flip basedToJSON uri) xs

-- | A value that can be deserialized from JSON, respecting a base URI.
class BasedFromJSON a where
    basedParseJSON :: Value -> URI -> A.Parser a
instance BasedFromJSON String where
    basedParseJSON = ignoreBaseInJSON
instance BasedFromJSON Int where
    basedParseJSON = ignoreBaseInJSON
instance BasedFromJSON Bool where
    basedParseJSON = ignoreBaseInJSON

-- | Implementation of 'basedParseJSON' for types that don't care about the URI.
ignoreBaseInJSON :: (A.FromJSON a) => Value -> URI -> A.Parser a
ignoreBaseInJSON v _ = A.parseJSON v

-- | A value that can be deserialized from a Scotty param, respecting a base URI.
class BasedParsable a where
    basedParseParam :: LT.Text -> URI -> Either LT.Text a
instance BasedParsable String where
    basedParseParam = ignoreBaseInParam
instance BasedParsable Int where
    basedParseParam = ignoreBaseInParam
instance BasedParsable Bool where
    basedParseParam = ignoreBaseInParam

-- | Implementation of 'basedParseParam' for types that don't care about the URI.
ignoreBaseInParam :: (Parsable a) => LT.Text -> URI -> Either LT.Text a
ignoreBaseInParam t _ = parseParam t

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
    'template' method of its 'RequestParsable' instance; usually you
    will have defined an instance like

    @
        instance 'RequestParsable' MyType where
            template = MyType \<$\> parse \"id\" \<*\> parse \"name\"
    @

    The 'URI' to be supplied to the 'ReaderT' wrapper is the base URI
    of the app (which might affect the interpretation of URLs in the
    body of the request, for example).

    If an error occurs parsing the request, a 400 (Bad Request)
    response is thrown.
-}
body :: (RequestParsable a) => ReaderT URI ActionStatusM a
body = do
    uri <- ask
    lift $ fromRequestBody template uri

{- |
    Use the given 'RequestParser' to interpret the data in the HTTP
    request.  See the discussion in 'body' for the usual method
    of defining a 'RequestParser'.

    If an error occurs parsing the request, a 400 (Bad Request)
    response is thrown.
-}
fromRequestBody :: RequestParser a -> URI -> ActionStatusM a
fromRequestBody expr uri = do
    kvs <- requestData
    case evaluateE (runReaderT expr uri) kvs of
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
    An object that can be constructed by a 'RequestParser'.
    Typically the 'template' method should be implemented as an
    applicative expression; see the example under 'body'.

    The type signatures here are more general than 'RequestParser',
    to faciliate future use with other functors, e.g., to generate
    HTML forms and documentation from the same applicative expression.
-}
class RequestParsable a where
    template :: ( Applicative f
                , Holes LT.Text ParamValue (ReaderT URI f)
                , Errs [EvaluationError LT.Text ParamValue] (ReaderT URI f)
                )
             => ReaderT URI f a

{- |
    An error encountered when evaluating a 'RequestParsable' object
    by filling in values for its 'hole's and calling 'evaluateE'.
    (This occurs in 'body', for example, where the values come from
    the HTTP request body.)
-}
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
-- (Combinator for use in 'template' implementations.)
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
type RequestParser = ReaderT URI
    (EvaluatorE LT.Text ParamValue [EvaluationError LT.Text ParamValue])
instance Holes LT.Text ParamValue RequestParser where
    hole = ReaderT . const . hole
instance Errs [EvaluationError LT.Text ParamValue] RequestParser where
    throwLeft = mapReaderT throwLeft
    catch fa handle = ReaderT $ \uri ->
        (runReaderT fa uri) `catch` (\e -> runReaderT (handle e) uri)

-- | How to report missing keys in a 'RequestParser'.
instance MissingKeyError LT.Text [EvaluationError LT.Text ParamValue] where
    missingKeyError k = [MissingKey k]

-- | A named hole which parses a 'ParamValue' into the appropriate type.
-- (Combinator for use in 'template' implementations.)
parse :: ( Functor f
         , BasedParsable a, BasedFromJSON a
         , Holes k ParamValue (ReaderT URI f)
         , Errs [EvaluationError k ParamValue] (ReaderT URI f)
         )
      => k -> ReaderT URI f a
parse k =
    let basedParse = flip $ basedParseEither k
        enlist (Left e) = Left [e]
        enlist (Right a) = Right a
    in throwLeft $ enlist <$>
        (ReaderT $ fmap . basedParse <*> runReaderT (hole k))

-- | Parse a 'ParamValue' into the appropriate type.
basedParseEither :: (BasedParsable a, BasedFromJSON a)
            => k -> ParamValue -> URI -> Either (EvaluationError k ParamValue) a
basedParseEither k x@(ScottyParam txt) uri =
    case basedParseParam txt uri of
        Left e -> Left $ ParseError k x e
        Right a -> Right a
basedParseEither k x@(JSONField jsonval) uri =
    case runJSONParser (basedParseJSON jsonval uri) of
        Left e -> Left $ ParseError k x $ LT.pack e
        Right a -> Right a
    where runJSONParser :: A.Parser a -> Either String a
          runJSONParser p = A.parseEither (const p) ()

-- | The values obtainable from an HTTP request.
data ParamValue = ScottyParam LT.Text
                | JSONField Value
    deriving (Eq, Show)
