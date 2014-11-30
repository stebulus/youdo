{-# LANGUAGE OverloadedStrings #-}
module YouDo.Web.Test where
import Blaze.ByteString.Builder (toLazyByteString)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, takeMVar, putMVar,
    modifyMVar_, modifyMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT(..), left, right)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.CaseInsensitive (mk)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Monoid(..))
import Distribution.TestSuite (Test(..), TestInstance(..), Progress(..),
    Result(..))
import Network.HTTP.Types (Status, ResponseHeaders, ok200, created201, http11,
    methodGet, methodPost, decodePathSegments, parseQuery, Method)
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
        (stat, _, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assignerid=0&assigneeid=0&description=blah&duedate=&completed=false"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        stat ~= created201
    ]

type TestResult = EitherT String IO ()
failure :: String -> TestResult
failure = left
success :: TestResult
success = right ()

(~=) :: (Eq a, Show a) => a -> a -> TestResult
x ~= y = if x == y
         then success
         else failure $ "expected " ++ (show y) ++ ", got " ++ (show x)
infix 1 ~=

serverTest :: (IsRequest a) =>
    String
    -> ((a -> IO (Status, ResponseHeaders, ByteString)) -> TestResult)
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

method :: Method -> RequestTransformer
method meth = RequestTransformer $ \ioreq -> do
    req <- ioreq
    return req { requestMethod = meth }

url :: String -> RequestTransformer
url u = RequestTransformer $ \ioreq -> do
    req <- ioreq
    let req' = if httpVersion req == http11
               then hostless { requestHeaders = assoc (mk "Host") host
                                    $ requestHeaders hostless
                             , requestHeaderHost = Just (host <> portstr)
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
        parseduri = fromJust $ parseURI u
    return req'

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
header key val = RequestTransformer $ \ioreq -> do
    req <- ioreq
    return req { requestHeaders = (mk key, val) : requestHeaders req }

request :: (IsRequest a) =>
    Application -> a -> IO (Status, ResponseHeaders, ByteString)
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
