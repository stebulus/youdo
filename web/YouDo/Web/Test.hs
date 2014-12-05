{-# LANGUAGE OverloadedStrings #-}
module YouDo.Web.Test where
import Blaze.ByteString.Builder (toLazyByteString)
import Control.Applicative ((<$>))
import Control.Concurrent.MVar (newEmptyMVar, newMVar, takeMVar, putMVar,
    modifyMVar_, modifyMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT(..), left, right, hoistEither)
import Data.Aeson (eitherDecode, Object, Value(..))
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as SB
import Data.CaseInsensitive (mk)
import qualified Data.HashMap.Strict as M
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Monoid(..))
import qualified Data.Text as T
import Distribution.TestSuite (Test(..), TestInstance(..), Progress(..),
    Result(..))
import Network.HTTP.Types (Status, ResponseHeaders, ok200, created201,
    badRequest400, methodNotAllowed405, http11, methodGet, methodPost,
    methodPut, decodePathSegments, parseQuery, Method)
import Network.URI (parseURI, URI(..), URIAuth(..))
import Network.Wai (Application, responseToStream, RequestBodyLength(..), requestBody,
    defaultRequest)
import Network.Wai.Internal (Request(..), ResponseReceived(..))
import Web.Scotty (scottyApp)
import YouDo.Web (app, withDB, DBOption(..))

tests :: IO [Test]
tests = return
    [ serverTest "silly" $ \req -> do
        (stat, _, bod) <- liftIO $ req $ get "http://example.com/"
        stat ~= ok200
        bod ~= "placeholder"
    , serverTest "new youdo" $ \req -> do
        (stat, headers, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assignerid=0&assigneeid=0&description=blah&duedate=&completed=false"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        stat ~= created201
        let ydurl = SB.unpack $ fromJust $ lookup (mk "Location") headers
        (stat', headers', bod) <- liftIO $ req
            $ get ydurl
            <> header "Accept" "text/plain"
        stat' ~= ok200
        lookup (mk "Content-Type") headers'
            ~= Just "application/json; charset=utf-8"
        obj <- hoistEither (eitherDecode bod :: Either String Object)
        M.lookup "id" obj ~= Just (Number 1)
        M.lookup "assignerid" obj ~= Just (Number 0)
        M.lookup "assigneeid" obj ~= Just (Number 0)
        M.lookup "description" obj ~= Just (String "blah")
        M.lookup "duedate" obj ~= Just Null
        M.lookup "completed" obj ~= Just (Bool False)
        M.lookup "url" obj ~= (Just $ String $ T.pack ydurl)
        M.lookup "thisVersion" obj ~= (Just $ String $ T.pack $ ydurl <> "/1")
    , serverTest "new youdo with duedate" $ \req -> do
        -- In the request and the response the duedate is in the
        -- same format that JavaScript's Date.toJSON method returns,
        -- which is ISO 8601.
        (stat, headers, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assignerid=0&assigneeid=0&description=blah&\
                    \duedate=2014-11-30T14:10:05.038Z&completed=false"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        stat ~= created201
        let ydurl = SB.unpack $ fromJust $ lookup (mk "Location") headers
        (stat', headers', bod) <- liftIO $ req
            $ get ydurl
            <> header "Accept" "text/plain"
        stat' ~= ok200
        lookup (mk "Content-Type") headers'
            ~= Just "application/json; charset=utf-8"
        obj <- hoistEither (eitherDecode bod :: Either String Object)
        M.lookup "id" obj ~= Just (Number 1)
        M.lookup "assignerid" obj ~= Just (Number 0)
        M.lookup "assigneeid" obj ~= Just (Number 0)
        M.lookup "description" obj ~= Just (String "blah")
        M.lookup "duedate" obj ~= Just (String "2014-11-30T14:10:05.038Z")
        M.lookup "completed" obj ~= Just (Bool False)
        M.lookup "url" obj ~= (Just $ String $ T.pack ydurl)
    , serverTest "new youdo with bad content-type" $ \req -> do
        (stat, _, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assignerid=0&assigneeid=0&description=blah&\
                    \duedate=2014-11-30T14:10:05.038Z&completed=false"
            <> header "Content-Type" "snee"
        stat ~= badRequest400
    , serverTest "new youdo with unparsable assignerid" $ \req -> do
        (stat, _, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assignerid=f&assigneeid=0&description=blah&\
                    \duedate=2014-11-30T14:10:05.038Z&completed=false"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        stat ~= badRequest400
    , serverTest "new youdo with missing assignerid" $ \req -> do
        (stat, _, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assigneeid=0&description=blah&\
                    \duedate=2014-11-30T14:10:05.038Z&completed=false"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        stat ~= badRequest400
    , serverTest "bad method to /0/youdos" $ \req -> do
        (stat, hdrs, _) <- liftIO $ req
            $ method methodPut
            <> url "http://example.com/0/youdos"
            <> body "grar"
            <> header "Content-Type" "text/plain"
        stat ~= methodNotAllowed405
        (sort . unintersperse ',' . filter (/= ' ') . SB.unpack)
            <$> lookup (mk "Allow") hdrs
            ~= Just ["GET", "POST"]
    ]

unintersperse :: (Eq a) => a -> [a] -> [[a]]
unintersperse _ [] = []
unintersperse d xs =
    let (hd,tl) = break (== d) xs
    in hd : case tl of
                [] -> []
                _:tl' -> unintersperse d tl'

type TestResult = EitherT String IO ()
failure :: String -> TestResult
failure = left
success :: TestResult
success = right ()

-- Assert equality.
(~=) :: (Eq a, Show a) => a -> a -> TestResult
x ~= y = if x == y
         then success
         else failure $ "expected " ++ (show y) ++ ", got " ++ (show x)
infix 1 ~=

-- A named test which makes requests to a transient in-memory YouDo
-- server.  The test proper is the second argument, a function which
-- accepts a requesting function and uses it to perform a test,
-- returning a TestResult.  The requesting function accepts any
-- instance of IsRequest; normally this will be a RequestTransformer
-- constructed by chaining together values returned by helper functions
-- such as get, post, header, and body.  See the invocations of
-- serverTest above for examples.
serverTest :: (IsRequest a) =>
    String
    -> ((a -> IO (Status, ResponseHeaders, LB.ByteString)) -> TestResult)
    -> Test
serverTest testName f = Test $ TestInstance
    { run = withDB InMemory $ \db -> do
        mvdb <- newMVar db
        waiApp <- scottyApp $ app (fromJust $ parseURI "http://example.com") mvdb
        result <- runEitherT $ f $ request waiApp
        return $ Finished $ case result of
            Left msg -> Fail msg
            Right _ -> Pass
    , name = testName
    , tags = []
    , options = []
    , setOption = \_ _ -> Left "no options supported"
    }

class IsRequest a where
    toRequest :: a -> IO Request

instance IsRequest Request where
    toRequest = return

data RequestTransformer = RequestTransformer (IO Request -> IO Request)
instance IsRequest RequestTransformer where
    toRequest (RequestTransformer xfm) =
        xfm $ return defaultRequest { httpVersion = http11 }
instance Monoid RequestTransformer where
    mempty = RequestTransformer id
    mappend (RequestTransformer f) (RequestTransformer g)
        = RequestTransformer (f.g)

transform :: (Request -> Request) -> RequestTransformer
transform f = RequestTransformer $ fmap f

method :: Method -> RequestTransformer
method meth = transform $ \r -> r { requestMethod = meth }

url :: String -> RequestTransformer
url u = transform $ \req ->
    let req' = if httpVersion req == http11
               then hostless { requestHeaders = assoc (mk "Host") fullhost
                                    $ requestHeaders hostless
                             , requestHeaderHost = Just fullhost
                             }
               else hostless
        hostless = req { pathInfo = decodePathSegments path
                       , rawPathInfo = path
                       , rawQueryString = query
                       , queryString = parseQuery query
                       }
        query = SB.pack $ uriQuery parseduri
        path = SB.pack $ uriPath parseduri
        auth = fromJust $ uriAuthority parseduri
        host = SB.pack $ uriRegName auth
        portstr = SB.pack $ uriPort auth
        fullhost = host <> portstr
        parseduri = fromJust $ parseURI u
    in req'

get :: String -> RequestTransformer
get u = method methodGet <> url u

post :: String -> RequestTransformer
post u = method methodPost <> url u

body :: SB.ByteString -> RequestTransformer
body s = RequestTransformer $ \ioreq -> do
    req <- ioreq
    -- The IO action in requestBody in Request is supposed to produce
    -- successive chunks, and "" when there aren't any more.  Thus:
    iobody <- successively $ s : repeat ""
    return req { requestBody = iobody
               , requestBodyLength = KnownLength $ fromIntegral $ SB.length s
               }

header :: SB.ByteString -> SB.ByteString -> RequestTransformer
header key val = transform
    $ \r -> r { requestHeaders = (mk key, val) : requestHeaders r }

-- Get the response of the given Application to the given Request.
request :: (IsRequest a) =>
    Application -> a -> IO (Status, ResponseHeaders, LB.ByteString)
request appl req = do
    mvresp <- newEmptyMVar
    mvbody <- newMVar ""
    req' <- toRequest req
    _ <- appl req' $ \resp -> do
        let (status, hdrs, stream) = responseToStream resp
        stream (\streamingbody ->
            let write builder = modifyMVar_ mvbody
                    (\x -> return (x <> toLazyByteString builder))
                flush = return ()
            in streamingbody write flush)
        bod <- takeMVar mvbody
        putMVar mvresp (status, hdrs, bod)
        return ResponseReceived
    takeMVar mvresp

-- Construct an IO action that returns successive elements from a list.
successively :: [a] -> IO (IO a)
successively xs = do
    mv <- newMVar xs
    return $ modifyMVar mv (\(x:rest) -> return (rest,x))

-- Update or insert an entry in an associative list.
assoc :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
assoc k v [] = [(k,v)]
assoc k v ((k',v') : rest)
    | k == k' = (k,v) : rest
    | otherwise = (k',v') : assoc k v rest
