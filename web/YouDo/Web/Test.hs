{-# LANGUAGE OverloadedStrings #-}
module YouDo.Web.Test where
import Blaze.ByteString.Builder (toLazyByteString)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, takeMVar, putMVar,
    modifyMVar_)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (fromJust)
import Data.Monoid (mempty, (<>))
import Distribution.TestSuite (Test(..), TestInstance(..), Progress(..),
    Result(..))
import Network.HTTP.Types (Status, ResponseHeaders, ok200, http11, methodGet)
import Network.URI (parseURI)
import Network.Wai (Application, Request(..), Response, responseToStream,
    defaultRequest)
import Network.Wai.Internal (ResponseReceived(..))
import Web.Scotty (scottyApp)
import YouDo.DB.Mock (empty)
import YouDo.Web (app, withDB, DBOption(..))

tests :: IO [Test]
tests = return [sillyTest]

sillyTest :: Test
sillyTest = Test $ TestInstance
    { run = withDB InMemory $ \db -> do
        mvdb <- newMVar db
        wsgiapp <- scottyApp $ app (fromJust $ parseURI "http://example.com") mvdb
        (stat, hdrs, body) <- request wsgiapp
                defaultRequest
                    { requestMethod = methodGet
                    , httpVersion = http11
                    , rawPathInfo = "/"
                    }
        return $ Finished $
            if stat /= ok200
            then Fail (show stat)
            else if body /= "placeholder"
            then Fail (unpack body)
            else Pass
    , name = "silly"
    , tags = []
    , options = []
    , setOption = \_ _ -> Left "no options supported"
    }

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
