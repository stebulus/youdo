{-# LANGUAGE OverloadedStrings #-}
module YouDo.Web.Test where
import Blaze.ByteString.Builder (toLazyByteString)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, takeMVar, putMVar,
    modifyMVar_, modifyMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT(..), left, right)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.CaseInsensitive (CI, mk)
import Data.Maybe (fromJust)
import Data.Monoid (mempty, (<>))
import Distribution.TestSuite (Test(..), TestInstance(..), Progress(..),
    Result(..))
import Network.HTTP.Types (Status, ResponseHeaders, ok200, created201, http11,
    methodGet, methodPost, decodePathSegments, parseQuery, Method)
import Network.Socket (SockAddr(..))
import Network.URI (parseURI, URI(..), URIAuth(..))
import Network.Wai (Application, responseToStream, RequestBodyLength(..), requestBody)
import Network.Wai.Internal (Request(..), ResponseReceived(..))
import Web.Scotty (scottyApp)
import YouDo.Web (app, withDB, DBOption(..))

tests :: IO [Test]
tests = return
    [ sillyTest
    , serverTest "new youdo" $ \req -> do
        thereq <- liftIO
            $ setBody "assignerid=0&assigneeid=0&description=blah&duedate=&completed=false"
            $ addHeader "Content-Type" "application/x-www-form-urlencoded"
            $ basicRequest methodPost "http://example.com/0/youdos"
        (stat, _, _) <- liftIO $ req thereq
        stat ~= created201
    ]

sillyTest :: Test
sillyTest = serverTest "silly" $ \req -> do
    (stat, _, body) <- liftIO $ req $
        basicRequest methodGet "http://example.com/"
    stat ~= ok200
    body ~= "placeholder"

(~=) :: (Eq a, Show a, Monad m) => a -> a -> EitherT String m ()
x ~= y = if x == y
         then right ()
         else left $ "expected " ++ (show y) ++ ", got " ++ (show x)
infix 1 ~=

serverTest :: String
    -> ((Request -> IO (Status, ResponseHeaders, ByteString))
        -> EitherT String IO ())
    -> Test
serverTest testName f = Test $ TestInstance
    { run = withDB InMemory $ \db -> do
        mvdb <- newMVar db
        waiApp <- scottyApp $ app (fromJust $ parseURI "http://example.com") mvdb
        result <- runEitherT $ f $ request waiApp . return
        return $ Finished $ case result of
            Left msg -> Fail msg
            Right _ -> Pass
    , name = testName
    , tags = []
    , options = []
    , setOption = \_ _ -> Left "no options supported"
    }

basicRequest :: Method -> String -> Request
basicRequest method uri = Request
    { requestMethod = method
    , httpVersion = http11
    , pathInfo = decodePathSegments path
    , rawPathInfo = path
    , rawQueryString = query
    , queryString = parseQuery query
    , requestHeaders = [("Host"::CI SB.ByteString, host)]
    , isSecure = False
    , remoteHost = SockAddrInet 0 0
    , requestBody = return ""
    , vault = mempty
    , requestBodyLength = KnownLength $ fromIntegral $ SB.length body
    , requestHeaderHost = Just host
    , requestHeaderRange = Nothing
    }
    where body = ""::SB.ByteString
          host = SB.pack $ uriRegName $ fromJust $ uriAuthority parseduri
          query = SB.pack $ uriQuery parseduri
          path = SB.pack $ uriPath parseduri
          parseduri = fromJust $ parseURI uri

setBody :: String -> Request -> IO Request
setBody body req = do
    -- The IO action in requestBody in Request is supposed to produce
    -- successive chunks, and "" when there aren't any more.  Thus:
    iobody <- successively $ (SB.pack body) : repeat ""
    return req
        { requestBody = iobody
        , requestBodyLength = KnownLength $ fromIntegral $ length body
        }

addHeader :: SB.ByteString -> SB.ByteString -> Request -> Request
addHeader key val req =
    req { requestHeaders = (mk key, val) : requestHeaders req }

request :: Application -> IO Request -> IO (Status, ResponseHeaders, ByteString)
request appl ioreq = do
    mvresp <- newEmptyMVar
    mvbody <- newMVar ""
    req <- ioreq
    _ <- appl req $ \resp -> do
        let (status, hdrs, stream) = responseToStream resp
        stream (\streamingbody ->
            let write builder = modifyMVar_ mvbody
                    (\x -> return (x <> toLazyByteString builder))
                flush = return ()
            in streamingbody write flush)
        body <- takeMVar mvbody
        putMVar mvresp (status, hdrs, body)
        return ResponseReceived
    takeMVar mvresp

-- Construct an IO action that returns successive elements from a list.
successively :: [a] -> IO (IO a)
successively xs = do
    mv <- newMVar xs
    return $ modifyMVar mv (\(x:rest) -> return (rest,x))
