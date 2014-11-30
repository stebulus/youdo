{-# LANGUAGE OverloadedStrings #-}
module YouDo.Web.Test where
import Blaze.ByteString.Builder (toLazyByteString)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, takeMVar, putMVar,
    modifyMVar_)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Char8 as SB
import Data.CaseInsensitive (CI)
import Data.Maybe (fromJust)
import Data.Monoid (mempty, (<>))
import Distribution.TestSuite (Test(..), TestInstance(..), Progress(..),
    Result(..))
import Network.HTTP.Types (Status, ResponseHeaders, ok200, http11, methodGet,
    decodePathSegments, parseQuery, Method)
import Network.Socket (SockAddr(..))
import Network.URI (parseURI, URI(..), URIAuth(..))
import Network.Wai (Application, Response, responseToStream, defaultRequest,
    RequestBodyLength(..))
import Network.Wai.Internal (Request(..), ResponseReceived(..))
import Web.Scotty (scottyApp)
import YouDo.DB.Mock (empty)
import YouDo.Web (app, withDB, DBOption(..))

tests :: IO [Test]
tests = return [sillyTest]

sillyTest :: Test
sillyTest = serverTest "silly" $ \req -> do
    (stat, hdrs, body) <- req $ basicRequest methodGet "http://example.com/"
    return $ Finished $
        if stat /= ok200
        then Fail (show stat)
        else if body /= "placeholder"
        then Fail (unpack body)
        else Pass

serverTest :: String
    -> ((Request -> IO (Status, ResponseHeaders, ByteString)) -> IO Progress)
    -> Test
serverTest testName f = Test $ TestInstance
    { run = withDB InMemory $ \db -> do
        mvdb <- newMVar db
        waiApp <- scottyApp $ app (fromJust $ parseURI "http://example.com") mvdb
        f (request waiApp)
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

request :: Application -> Request -> IO (Status, ResponseHeaders, ByteString)
request appl req = do
    mvresp <- newEmptyMVar
    mvbody <- newMVar ""
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
