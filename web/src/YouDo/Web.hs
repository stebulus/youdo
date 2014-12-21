{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts,
    FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies,
    ScopedTypeVariables #-}
{-|
Module      : YouDo.Web
Description : A tiny web framework on top of Scotty.
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.Web (
    -- * Base and relative URIs
    BasedFromJSON(..), BasedParsable(..),
    -- * Interpreting requests
    fromRequestBody, RequestParser, ParamValue(..), requestData,
    EvaluationError(..), optional,
    HasCaptureDescr(..), HasJSONDescr(..),
    FromRequestBody(..), FromRequestBodyContext(..), FromRequestContext(..),
) where

import Codec.MIME.Type (mimeType, MIMEType(Application))
import Codec.MIME.Parse (parseMIMEType)
import Control.Applicative ((<$>), Applicative(..), Const(..))
import Control.Monad.Reader (ReaderT(..), ask, mapReaderT)
import Control.Monad.Trans (MonadTrans(..))
import Data.Aeson (Value(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as M
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (unsupportedMediaType415)
import Network.URI (URI(..))
import Web.Scotty (header, param, params, Parsable(..))
import qualified Web.Scotty as Scotty

import YouDo.Const
import YouDo.Holes
import YouDo.Web.ActionM

class HasCaptureDescr a where
    captureDescr :: Maybe a -> LT.Text
    addCaptureDescr :: Const LT.Text a -> Const LT.Text a
    addCaptureDescr c = addConst c $ captureDescr x
        where x = Nothing :: Maybe a

class HasJSONDescr a where
    jsonDescr :: Maybe a -> LT.Text
    addJSONDescr :: Const LT.Text a -> Const LT.Text a
    addJSONDescr c = addConst c $ jsonDescr x
        where x = Nothing :: Maybe a

instance HasJSONDescr String where
    jsonDescr = const $ "string"
instance HasJSONDescr Int where
    jsonDescr = const $ "integer"
instance HasJSONDescr Bool where
    jsonDescr = const $ "boolean"

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

{- |
    Use the given 'RequestParser' to interpret the data in the HTTP
    request.

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
    A way to construct values of type @a@ from the body of an HTTP
    request, in context @f@.

    Typically the 'template' method should be implemented as an
    applicative expression; see 'YouDo.Types' for examples.
-}
class FromRequestBody f a where
    template :: f a

{- |
    The functor @f@ represents a context within which we can make
    some use of a way to construct values of type @a@ from the body
    of an HTTP request.  See 'FromRequestBody'.
-}
class FromRequestBodyContext f where
    {- |
        In 'template' implementations, represents reading the value
        for the given key and parsing it as a value of type @a@.

        There is no provision for reporting errors in this interface,
        because what "error" means depends on the functor $f$.
        (For example, a functor that actually produces a value of
        type @a@ needs to be able to report the error that there is
        no value for the given key, whereas a functor that produces
        documentation describing the template doesn't.)
    -}
    parse :: (BasedParsable a, BasedFromJSON a, HasJSONDescr a)
          => LT.Text      -- ^The key whose value should be parsed.
          -> f a

    {- |
        In 'template' implementations, represents that if the
        given @f a@ has no value because the key given to 'parse'
        was not present in the request, then the given value should
        be substituted.
    -}
    defaultTo :: (Show a) => f a -> a -> f a

class (FromRequestBodyContext g) => FromRequestContext f g | f->g where
    {-  Obsolete docs.

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
    capture :: (Parsable a, HasCaptureDescr a) => LT.Text -> f a

    {-  Obsolete docs.

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
    body :: (FromRequestBody g a) => f a

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

-- | If the argument failed because it depends on a missing key,
-- replace it with 'Nothing'; otherwise, 'Just' the value in the
-- first argument.
optional :: (Functor f, FromRequestBodyContext f, Show a)
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

instance FromRequestBodyContext RequestParser where
    parse k =
        let basedParse = flip $ basedParseEither k
            enlist (Left e) = Left [e]
            enlist (Right a) = Right a
        in throwLeft $ enlist <$>
            (ReaderT $ fmap . basedParse <*> runReaderT (hole k))
    defaultTo fa d = fa `catch` \es ->
        case es of
            [MissingKey _] -> pure d
            _ -> throw $ pure es

instance FromRequestContext (ReaderT URI ActionStatusM) RequestParser where
    capture k = lift $ lift500 $ param k
    body = do
        uri <- ask
        lift $ fromRequestBody template uri

-- | How to report missing keys in a 'RequestParser'.
instance MissingKeyError LT.Text [EvaluationError LT.Text ParamValue] where
    missingKeyError k = [MissingKey k]

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
