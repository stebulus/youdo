{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module YouDo.Web.Test where
import Blaze.ByteString.Builder (toLazyByteString)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, takeMVar, putMVar,
    modifyMVar_, modifyMVar)
import Control.Exception (SomeException, handle)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Either (EitherT(..), left, right, hoistEither)
import Data.Aeson (eitherDecode, Object, Value(..), parseJSON)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as SB
import Data.CaseInsensitive (mk)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Monoid(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as V
import Distribution.TestSuite (Test(..), TestInstance(..), Progress(..),
    Result(..))
import Network.HTTP.Types (Status, ResponseHeaders, ok200, created201,
    badRequest400, methodNotAllowed405, http11, methodGet, methodPost,
    methodPut, decodePathSegments, parseQuery, Method)
import Network.URI (parseURI, URI(..), URIAuth(..), nullURI)
import Network.Wai (Application, responseToStream, RequestBodyLength(..), requestBody,
    defaultRequest)
import Network.Wai.Internal (Request(..), ResponseReceived(..))
import Web.Scotty (scottyApp, parseParam)
import YouDo.DB.Memory
import YouDo.Holes
import YouDo.Test (plainTest)
import YouDo.Time.Parse
import YouDo.Types (DueDate, UserID(..))
import YouDo.Web.Request (ParamValue(..), parse, EvaluationError(..),
    BasedFromJSON(..), BasedParsable(..), RequestParser)
import YouDo.WebApp (app)

tests :: IO [Test]
tests = return $
    [ serverTest "new youdo, form data" $ \req -> do
        (stat, headers, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assigner=http://example.com/0/users/0\
                    \&assignee=http://example.com/0/users/0\
                    \&description=blah&duedate=&completed=false"
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
        M.lookup "assigner" obj ~= Just (String "http://example.com/0/users/0/")
        M.lookup "assignee" obj ~= Just (String "http://example.com/0/users/0/")
        M.lookup "description" obj ~= Just (String "blah")
        M.lookup "duedate" obj ~= Just Null
        M.lookup "completed" obj ~= Just (Bool False)
        M.lookup "url" obj ~= (Just $ String $ T.pack ydurl)
        M.lookup "thisVersion" obj ~= (Just $ String $ T.pack $ ydurl <> "1")
    , serverTest "new youdo, json body" $ \req -> do
        (stat, headers, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "{\"assigner\":\"http://example.com/0/users/0\",\
                    \\"assignee\":\"http://example.com/0/users/0\",\
                    \\"description\":\"blah\",\
                    \\"duedate\":null,\
                    \\"completed\":false}"
            <> header "Content-Type" "application/json"
        stat ~= created201
        let ydurl = SB.unpack $ fromJust $ lookup (mk "Location") headers
        (stat', headers', bod) <- liftIO $ req
            $ get ydurl
            <> header "Accept" "text/plain"
        stat' ~= ok200
        lookup (mk "Content-Type") headers'
            ~= Just "application/json; charset=utf-8"
        obj <- hoistEither (eitherDecode bod :: Either String Object)
        M.lookup "assigner" obj ~= Just (String "http://example.com/0/users/0/")
        M.lookup "assignee" obj ~= Just (String "http://example.com/0/users/0/")
        M.lookup "description" obj ~= Just (String "blah")
        M.lookup "duedate" obj ~= Just Null
        M.lookup "completed" obj ~= Just (Bool False)
        M.lookup "url" obj ~= (Just $ String $ T.pack ydurl)
        M.lookup "thisVersion" obj ~= (Just $ String $ T.pack $ ydurl <> "1")
    , serverTest "new youdo with duedate" $ \req -> do
        -- In the request and the response the duedate is in the
        -- same format that JavaScript's Date.toJSON method returns,
        -- which is ISO 8601.
        (stat, headers, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assigner=http://example.com/0/users/0\
                    \&assignee=http://example.com/0/users/0\
                    \&description=blah&duedate=2014-11-30T14:10:05.038Z\
                    \&completed=false"
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
        M.lookup "assigner" obj ~= Just (String "http://example.com/0/users/0/")
        M.lookup "assignee" obj ~= Just (String "http://example.com/0/users/0/")
        M.lookup "description" obj ~= Just (String "blah")
        M.lookup "duedate" obj ~= Just (String "2014-11-30T14:10:05.038Z")
        M.lookup "completed" obj ~= Just (Bool False)
        M.lookup "url" obj ~= (Just $ String $ T.pack ydurl)
    , serverTest "new youdo with duedate, json body" $ \req -> do
        (stat, headers, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "{\"assigner\":\"http://example.com/0/users/0\",\
                    \\"assignee\":\"http://example.com/0/users/0\",\
                    \\"description\":\"blah\",\
                    \\"duedate\":\"2014-11-30T14:10:05.038Z\",\
                    \\"completed\":false}"
            <> header "Content-Type" "application/json"
        stat ~= created201
        let ydurl = SB.unpack $ fromJust $ lookup (mk "Location") headers
        (stat', headers', bod) <- liftIO $ req
            $ get ydurl
            <> header "Accept" "text/plain"
        stat' ~= ok200
        lookup (mk "Content-Type") headers'
            ~= Just "application/json; charset=utf-8"
        obj <- hoistEither (eitherDecode bod :: Either String Object)
        M.lookup "assigner" obj ~= Just (String "http://example.com/0/users/0/")
        M.lookup "assignee" obj ~= Just (String "http://example.com/0/users/0/")
        M.lookup "description" obj ~= Just (String "blah")
        M.lookup "duedate" obj ~= Just (String "2014-11-30T14:10:05.038Z")
        M.lookup "completed" obj ~= Just (Bool False)
        M.lookup "url" obj ~= (Just $ String $ T.pack ydurl)
        M.lookup "thisVersion" obj ~= (Just $ String $ T.pack $ ydurl <> "1")
    , serverTest "new youdo with bad content-type" $ \req -> do
        (stat, _, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assigner=http://example.com/0/users/0\
                    \&assignee=http://example.com/0/users/0\
                    \&description=blah\
                    \&duedate=2014-11-30T14:10:05.038Z&completed=false"
            <> header "Content-Type" "snee"
        stat ~= badRequest400
    , serverTest "new youdo with unparsable assignerid" $ \req -> do
        (stat, _, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assigner=http://example.com/0/users/f\
                    \&assignee=http://example.com/0/users/0\
                    \&description=blah\
                    \&duedate=2014-11-30T14:10:05.038Z&completed=false"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        stat ~= badRequest400
    , serverTest "new youdo with missing assignerid" $ \req -> do
        (stat, _, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assignee=http://example.com/0/users/0&description=blah&\
                    \duedate=2014-11-30T14:10:05.038Z&completed=false"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        stat ~= badRequest400
    , serverTest "new youdo with misformatted assignerid" $ \req -> do
        (stat, _, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assigner=http://example.com/0/users/q\
                    \&assignee=http://example.com/0/users/0&description=blah&\
                    \duedate=2014-11-30T14:10:05.038Z&completed=false"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        stat ~= badRequest400
    , serverTest "new youdo with misformatted completed" $ \req -> do
        (stat, _, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assigner=http://example.com/0/users/0\
                    \&assignee=http://example.com/0/users/0\
                    \&description=blah\
                    \&duedate=2014-11-30T14:10:05.038Z&completed=snee"
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
    , serverTest "youdo versions" $ \req -> do
        -- create a new youdo
        (stat1, headers, _) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assigner=http://example.com/0/users/0\
                    \&assignee=http://example.com/0/users/0\
                    \&description=blah&duedate=&completed=false"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        stat1 ~= created201
        let ydurl = SB.unpack $ fromJust $ lookup (mk "Location") headers
        -- check versions
        (stat2, _, bod) <- liftIO $ req $ get $ ydurl ++ "versions"
        stat2 ~= ok200
        objs2 <- hoistEither (eitherDecode bod :: Either String [Object])
        map (! "thisVersion") objs2 ~= [ String "http://example.com/0/youdos/1/1" ]
        -- change the youdo
        let String urltext3 = objs2!!0 ! "thisVersion"
        (stat3, hdrs3, _) <- liftIO $ req
            $ post (T.unpack urltext3)
            <> body "completed=true"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        stat3 ~= created201
        lookup (mk "Location") hdrs3 ~= Just "http://example.com/0/youdos/1/2"
        -- check versions
        (stat4, _, bod4) <- liftIO $ req $ get $ ydurl ++ "versions"
        stat4 ~= ok200
        objs4 <- hoistEither (eitherDecode bod4 :: Either String [Object])
        map (! "thisVersion") objs4 ~= [ String "http://example.com/0/youdos/1/2"
                                       , String "http://example.com/0/youdos/1/1"
                                       ]
        -- check correctness of new version
        let String urltext5 = objs4!!0 ! "thisVersion"
        (stat5, _, bod5) <- liftIO $ req $ get $ T.unpack urltext5
        stat5 ~= ok200
        obj5 <- hoistEither (eitherDecode bod5 :: Either String Object)
        M.lookup "assigner" obj5 ~= Just (String "http://example.com/0/users/0/")
        M.lookup "assignee" obj5 ~= Just (String "http://example.com/0/users/0/")
        M.lookup "description" obj5 ~= Just (String "blah")
        M.lookup "duedate" obj5 ~= Just Null
        M.lookup "completed" obj5 ~= Just (Bool True)
        M.lookup "url" obj5 ~= (Just $ String $ T.pack ydurl)
    ]

    -- Bad duedates in various parsing contexts.
    ++ do  -- List monad!
        (datestr, descr, errmsg)
            <- [ ( "2014-11-30T14_10:05.038Z", "misformatted date",
                 "parsing UTC time / parsing time / expected ':' / \
                 \Failed reading: satisfy / next characters: _10:05.038" )
               , ( "snee", "completely misformatted date",
                 "parsing UTC time / parsing date / parsing year (YYYY) / \
                 \Failed reading: takeWhile1 / next characters: snee" )
               , ( "2014-11-31T14:10:05.038Z", "invalid date",
                 "parsing UTC time / Failed reading: no such date / \
                 \next characters: T14:10:05." )
               ]
        m <- [ serverTest ("new youdo, form data, " ++ descr) $ \req -> do
                    (stat, _, bod) <- liftIO $ req
                        $ post "http://example.com/0/youdos"
                        <> body (SB.pack
                                 ("assigner=http://example.com/0/users/0\
                                  \&assignee=http://example.com/0/users/0\
                                  \&description=blah\
                                  \&duedate=" ++ datestr ++ "&completed=false"))
                        <> header "Content-Type" "application/x-www-form-urlencoded"
                    stat ~= badRequest400
                    bod ~= LB.pack ("cannot parse parameter \"duedate\": " ++ errmsg ++ "\r\n")
             , serverTest ("new youdo, json data, " ++ descr) $ \req -> do
                    (stat, _, bod) <- liftIO $ req
                        $ post "http://example.com/0/youdos"
                        <> body (SB.pack
                                    ("{\"assigner\":\"http://example.com/0/users/0\",\
                                     \\"assignee\":\"http://example.com/0/users/0\",\
                                     \\"description\":\"blah\",\
                                     \\"duedate\":\"" ++ datestr ++ "\",\
                                     \\"completed\":false}"))
                        <> header "Content-Type" "application/json"
                    stat ~= badRequest400
                    bod ~= LB.pack ("cannot parse parameter \"duedate\": " ++ errmsg ++ "\r\n")
             , plainTest ("holex parsing, " ++ descr) $
                    evaluateNoURI (parse "duedate" :: RequestParser DueDate)
                              [("duedate", JSONField (String $ T.pack datestr))]
                        ~= Left [ParseError "duedate"
                                 (JSONField (String $ T.pack datestr))
                                 (LT.pack errmsg)]
             , plainTest ("aeson parsing, " ++ descr) $
                    (parseEither parseJSON (String $ T.pack datestr)
                    :: Either String DueDate)
                    ~= Left errmsg
             , plainTest ("scotty parsing, " ++ descr) $
                    (parseParam (LT.pack datestr) :: Either LT.Text DueDate)
                        ~= Left (LT.pack errmsg)
             ]
        return m

    ++
    [ plainTest "Holes parsing from Scotty parameters" $ do
        let expr = (+) <$> (parse "a") <*> (parse "b") :: RequestParser Int
            val = evaluateNoURI expr [("a", ScottyParam "1"), ("b", ScottyParam "-3")]
            val' = evaluateNoURI expr [("a", ScottyParam "1"), ("b", ScottyParam "q")]
        (val,val') ~= (Right (-2),
            Left [ParseError "b" (ScottyParam "q") "readEither: no parse"])
    , plainTest "Holes parsing from JSON fields" $ do
        let expr = (+) <$> (parse "a") <*> (parse "b") :: RequestParser Int
            val = evaluateNoURI expr [("a", JSONField (Number 1)),
                                      ("b", JSONField (Number (-3)))]
            val' = evaluateNoURI expr [("a", JSONField (Number 1)),
                                       ("b", JSONField (String "q"))]
        (val,val') ~= (Right (-2),
            Left [ParseError "b" (JSONField (String "q"))
                "when expecting a Int, encountered String instead"])
    ]
    ++ do  -- List monad!
        let Just baseuri = parseURI "http://example.com/0/"
        (dataname, userurl, result)
            <- [ ( "trailing slash", "http://example.com/0/users/3/", Right (UserID 3) )
               , ( "no trailing slash", "http://example.com/0/users/3", Right (UserID 3) )
               , ( "weird", "http://example.com/0/users/../users/3/", Right (UserID 3) )
               , ( "no ID", "http://example.com/0/users/", Left "invalid ID" )
               , ( "not number", "http://example.com/0/users/3q/", Left "invalid ID" )
               , ( "not even an URL", "grar", Left "invalid URL" )
               ]
        (fname, f) <- [ ( "BasedFromJSON", \s ->
                            parseEither (flip basedParseJSON baseuri) (String s) )
                      , ( "BasedParsable", \s ->
                            either (Left . LT.unpack)
                                   Right
                                   $ basedParseParam (LT.fromStrict s) baseuri
                            )
                      ]
        return $ plainTest ("UserID from URL, " ++ dataname ++ ", " ++ fname)
                           $ f userurl ~= result

    ++
    [ serverTest "transaction URLs" $ \req -> do
        beforePost <- liftIO getCurrentTime
        liftIO $ threadDelay 10000
        (stat, hdrs, bod) <- liftIO $ req
            $ post "http://example.com/0/youdos"
            <> body "assigner=http://example.com/0/users/0\
                    \&assignee=http://example.com/0/users/0\
                    \&description=blah\
                    \&duedate=2014-11-30T14:10:05.038Z&completed=false"
            <> header "Content-Type" "application/x-www-form-urlencoded"
        liftIO $ threadDelay 10000
        afterPost <- liftIO getCurrentTime
        stat ~= created201
        youdourl <- case lookup (mk "Location") hdrs of
            Nothing -> left "missing location header"
            Just h -> return $ SB.unpack h
        lookup (mk "Content-Type") hdrs
            ~= Just "application/json; charset=utf-8"
        obj <- hoistEither (eitherDecode bod :: Either String Object)
        txnurl <- case M.lookup "transaction" obj of
            Just (String thetxnurl) -> return thetxnurl
            Just x -> left $ "expected String, got " ++ (show x)
            Nothing -> left "no transaction field"
        (stat', _, bod') <- liftIO $ req $ get $ T.unpack txnurl
        stat' ~= ok200
        obj' <- hoistEither (eitherDecode bod' :: Either String Object)
        M.lookup "url" obj' ~= Just (String txnurl)
        case M.lookup "timestamp" obj' of
            Nothing -> failure "no timestamp field"
            Just (String timestr) ->
                case parseUTCTime (LT.fromStrict timestr) of
                    Left e -> failure $
                                "can't parse time " ++ (show timestr)
                                ++ ": " ++ e
                    Right t ->
                        if beforePost <= t && t <= afterPost
                            then success
                            else failure $
                                    "transaction time " ++ (show timestr)
                                    ++ " is not in expected range ["
                                    ++ (show beforePost) ++ ","
                                    ++ (show afterPost) ++ "]"
            Just x -> failure $ "expected String, got " ++ (show x)
        M.lookup "users" obj' ~= (Just $ Array $ V.fromList [])
        youdo <- case M.lookup "youdos" obj' of
            Just (Array a) -> do
                V.length a ~= 1
                case V.head a of
                    Object yobj -> return yobj
                    x -> left $ "expected object, got " ++ (show x)
            x -> left $ "expected array, got " ++ (show x)
        M.lookup "url" youdo ~= Just (String $ T.pack youdourl)
    ]

evaluateNoURI :: RequestParser a
              -> [(LT.Text,ParamValue)]
              -> Either [EvaluationError LT.Text ParamValue] a
evaluateNoURI expr kvs = evaluateE (runReaderT expr nullURI) kvs

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
    { run = do
        db <- empty
        waiApp <- scottyApp
                  $ app db $ fromJust $ parseURI "http://example.com"
        result <- handle (\(e::SomeException) -> return $ Left $ show e)
            $ runEitherT $ f $ request waiApp
        return $ Finished $ case result of
            Left msg -> Fail msg
            Right _ -> Pass
    , Distribution.TestSuite.name = testName
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
