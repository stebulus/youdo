{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, FlexibleContexts, OverloadedStrings,
    ScopedTypeVariables #-}
{-|
Module      : YouDo.DB
Description : Database types for YouDo
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.DB (
    -- *Database and web interface
    DB(..), Updater(..), webdb, NamedResource(..),
    resourceURL, resourceVersionURL, idjson, veridjson,
    -- *Versioning of database objects
    Versioned(..), VersionedID(..), TransactionID(..),
    -- *Results of database operations
    GetResult, CreateResult, UpdateResult,
    Result(..),
    NotFound(..),
    InvalidObject(..), invalidObject,
    NewerVersion(..), newerVersion,
    one, some
) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Concurrent.MVar
import Control.Monad (liftM)
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON(..), (.=), Value(..))
import qualified Data.HashMap.Strict as M
import Data.List (foldl')
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Network.HTTP.Types (ok200, created201, badRequest400, notFound404,
    conflict409, internalServerError500, StdMethod(..))
import Network.URI
import Web.Scotty (Parsable(..), ScottyM)

import YouDo.Web

{- |
    @d@ contains versioned key-value pairs of type @(k,v)@, which
    can be updated by objects of type @u@, all in monad @m@.

    Often 'Updater' @u v@ will hold (see "YouDo.DB.Memory", for
    example), but it's not required.
-}
class (Monad m, NamedResource k)
      => DB k v u m d | d->v, d->k, d->u, d->m where

    -- | Get the object with the specified key.
    get :: k -> d -> m (GetResult (Versioned k v))

    -- | Get the specified version of an object.
    getVersion :: VersionedID k -> d -> m (GetResult (Versioned k v))

    -- | Get all versions of the specified object.
    getVersions :: k -> d -> m (GetResult [Versioned k v])

    -- | Get all objects.
    getAll :: d -> m (GetResult [Versioned k v])

    -- | Store a new object.
    create :: v -> d -> m (CreateResult (Versioned k v))

    {- |
        Update an existing object.  The @u@ in the given @Versioned
        k u@ is the update to make.  The 'VersionedID' @k@ in the
        @Versioned k u@ identifies the /existing/ version that this
        is an update to; if that is no longer the current version,
        the update will fail and this function will return @Left
        (NewerVersion x)@, where @x :: Versioned k v@ is the current
        version.  It's up to the caller to determine whether their
        update should be attempted again on that new version, and if
        so, to retry.
    -}
    update :: (Versioned k u) -> d
        -> m (UpdateResult (Versioned k v) (Versioned k v))

    -- | The name of this resource.
    -- Obtained from the 'NamedResource' instance for 'k'.
    dbResourceName :: d -> String
    dbResourceName = const $ resourceName x
        where x = Nothing :: Maybe k    -- ScopedTypeVariables used!

{- |
    A @u@ can be used to change an @a@.
    In an instance 'DB' @k v u m d@, we might well have @Updater u v@
    (see "YouDo.DB.Memory", for example), but this is not required.
-}
class Updater u a where
    doUpdate :: u -> a -> a

{- |
    A web interface to an instance of 'DB'.
    The following endpoints are created, relative to the given base URI
    (which should probably end with a slash):

    @
         GET objs                (list of all current objs)
         POST objs               (create new obj)
         GET objs\//id/\/             (current version of obj)
         GET objs\//id/\/versions    (all versions of obj)
         GET objs\//id/\//txnid/       (specified version of obj)
         POST objs\//id/\//txnid/      (create new version of obj)
    @

    These correspond directly to the methods of 'DB'.  The name @objs@
    is obtained from the instance 'NamedResource' @k@.  Requests that
    return objects return them in JSON format, using the instances
    'Show' @k@ and 'BasedToJSON' @v@.  The @/id/@ parameter is interpreted
    via the instance 'Parsable' @k@.  (The 'FromJSON' @k@ instance
    would only be used if the id were passed in the JSON request
    body, which it shouldn't be.)  The request body, when needed,
    is interpreted via default 'RequestParser' for the appropriate type.
-}
webdb :: ( NamedResource k, DB k v u IO d
         , Parsable k, FromJSON k
         , Show k, BasedToJSON v
         , Constructible (RequestParser v)
         , Constructible (RequestParser k)
         , Constructible (RequestParser (VersionedID k))
         , Constructible (RequestParser (Versioned k u))
         ) => MVar ()     -- ^All database access is under this MVar.
         -> d           -- ^The database.
         -> Based ScottyM ()
webdb mv db = do
    let rtype = dbResourceName db
        onweb f = webfunc $ lock $ flip f db
        lock f x = withMVar mv $ const (f x)
    resource rtype
             [ (GET, onweb (\() -> getAll))
             , (POST, onweb create)
             ]
    resource (rtype ++ "/:id/")
             [ (GET, onweb get) ]
    resource (rtype ++ "/:id/versions")
             [ (GET, onweb getVersions) ]
    resource (rtype ++ "/:id/:txnid")
             [ (GET, onweb getVersion)
             , (POST, onweb update)
             ]

{- |
    Class for types that have a name.  Typically the implementation
    will ignore the argument, which is included only for type-checking.
    (So, it may be @Nothing@.)

    This name is used in the web interface to construct URLs for the
    resources that represent operations on objects of type @a@.  It's
    convenient to have this information be associated statically with
    the type because it's not just objects of the type itself which
    need to know it, but objects which refer to objects of the type.
    (For example, 'Youdo' contains some 'UserID's which should be
    sent to the client as URLs containing the name for user objects.)
-}
class NamedResource a where
    resourceName :: Maybe a -> String

-- | The relative URL for a 'NamedResource' object.
resourceRelativeURLString :: (Show k, NamedResource k) => k -> String
resourceRelativeURLString k = "./" ++ resourceName (Just k) ++ "/"
                              ++ show k ++ "/"

-- | The URL for a 'NamedResource' object.
resourceURL :: (Show k, NamedResource k, Monad m) => k -> Based m URI
resourceURL k = do
    baseuri <- ask
    return $ resourceRelativeURLString k `relative` baseuri

-- | The URL for a specific version of a 'NamedResource' object.
resourceVersionURL :: (Show k, NamedResource k, Monad m)
                      => VersionedID k -> Based m URI
resourceVersionURL verk = do
    baseuri <- ask
    return $ (resourceRelativeURLString (thingid verk)
                ++ (show $ txnid $ verk))
             `relative` baseuri

jsonurl :: URI -> Value
jsonurl = String . ST.pack . show

-- | The URL for a 'NamedResource' object, as a JSON 'Value'.
idjson :: (Show k, NamedResource k, Monad m) => k -> Based m Value
idjson k = liftM jsonurl $ resourceURL k

-- | The URL for a specific version of a 'NamedResource' object, as a JSON 'Value'.
veridjson :: (Show k, NamedResource k, Monad m) => VersionedID k -> Based m Value
veridjson k = liftM jsonurl $ resourceVersionURL k

{- |
    An identifier of a version of a thing, as was produced in a
    particular transaction.
-}
data VersionedID a = VersionedID
    { thingid :: a
    , txnid :: TransactionID
    } deriving (Show, Eq)

instance (Parsable a, FromJSON a, Constructor f)
        => Constructible (f (VersionedID a)) where
    construct = VersionedID <$> parse "id" <*> parse "txnid"

-- | A version of a thing, as was produced in a particular transaction.
data Versioned a b = Versioned
    { version :: VersionedID a
    , thing :: b
    } deriving (Show, Eq)

instance (Constructor f, Constructible (f (VersionedID a)), Constructible (f b))
        => Constructible (f (Versioned a b)) where
    construct = Versioned <$> construct <*> construct

-- | Augment JSON representations of 'Versioned' objects
-- with @"url"@ and @"thisVersion"@ fields.
instance (Show k, NamedResource k, BasedToJSON v)
         => BasedToJSON (Versioned k v) where
    basedToJSON v = do
        objurl <- resourceURL $ thingid $ version v
        verurl <- resourceVersionURL $ version v
        origval <- basedToJSON $ thing v
        let origmap = case origval of
                Object m -> m
                _ -> error "data did not encode as JSON object"
            augmentedmap = foldl' (flip (uncurry M.insert)) origmap
                [ "url" .= show objurl
                , "thisVersion" .= show verurl
                ]
        return $ Object augmentedmap

-- | Transaction identifier.
newtype TransactionID = TransactionID Int deriving (Eq)
instance Show TransactionID where
    show (TransactionID n) = show n
instance FromField TransactionID where
    fromField fld = (fmap.fmap) TransactionID $ fromField fld
instance Parsable TransactionID where
    parseParam x = TransactionID <$> parseParam x
instance FromJSON TransactionID where
    parseJSON x = TransactionID <$> parseJSON x

{- |
    @r@ represents the result of a 'DB' operation which was supposed
    to return a value of type @a@.
-}
class Result r a | r->a where
    notFound :: r
    failure :: LT.Text -> r
    success :: a -> r

{- |
    Result of a 'DB' operation: the desired object, or the targetted
    object, doesn't exist.
-}
data NotFound = NotFound

{- |
    Result of a 'DB' update operation: the targetted version is no
    longer the current version of the object.  The current version
    is enclosed.
-}
data NewerVersion a = NewerVersion a

{- |
    Result of a 'DB' create operation: the supplied data describes an
    invalid object.  A list of reasons is enclosed.
-}
data InvalidObject = InvalidObject [LT.Text]

-- | Result of a 'DB' operation that gets something: the object was
-- not found, some other error occurred, or the object is enclosed.
type GetResult a = Either NotFound (Either LT.Text a)
instance Result (GetResult a) a where
    notFound = Left NotFound
    failure msg = Right $ Left msg
    success x = Right $ Right x

instance (BasedToJSON a) => WebResult (GetResult a) where
    report (Right (Right a)) =
        do status ok200  -- http://tools.ietf.org/html/rfc2616#section-10.2.1
           json a
    report (Left NotFound) =
        status notFound404  -- http://tools.ietf.org/html/rfc2616#section-10.4.5
    report (Right (Left msg)) =
        do status internalServerError500  -- http://tools.ietf.org/html/rfc2616#section-10.5.1
           text msg

{- |
    Result of a 'DB' update operation.  The operation failed because
    a newer version exists (and that version is enclosed), the object
    was not found, some other error occurred, or the update succeeded
    and the updated object is enclosed.

    @a@ is the type of object being updated, when the operation
    succeeds and returns the updated version.  @b@ is the type of
    object being updated, when the operation fails because a newer
    version exists.  Normally @a=b@, but they must be treated as
    distinct, since otherwise an 'fmap' would have to apply to both
    the successful result and the failure result, contrary to the
    usual expectations for types representing result-or-error.
-}
type UpdateResult b a = Either (NewerVersion b) (GetResult a)
instance Result (UpdateResult b a) a where
    notFound = Right $ notFound
    failure = Right . failure
    success = Right . success
newerVersion :: b -> UpdateResult b a
newerVersion x = Left $ NewerVersion x

instance (BasedToJSON b, NamedResource k, Show k, BasedToJSON v)
         => WebResult (UpdateResult b (Versioned k v)) where
    report (Right (Right (Right a))) =
        do status created201  -- http://tools.ietf.org/html/rfc2616#section-10.2.2
           url <- resourceVersionURL $ version a
           setHeader "Location" $ LT.pack $ show $ url
           json a
    report (Left (NewerVersion b)) =
        do status conflict409  -- http://tools.ietf.org/html/rfc2616#section-10.4.10
           json b
    report (Right gr) = report gr

{- |
    Result of a 'DB' create operation.  The data supplied describes
    an invalid object (and reasons are enclosed), the object was not
    found (?), some other error occurred, or the create succeeded and
    the created object is enclosed.
-}
type CreateResult a = Either InvalidObject (GetResult a)
instance Result (CreateResult a) a where
    notFound = Right $ notFound
    failure = Right . failure
    success = Right . success
invalidObject :: [LT.Text] -> CreateResult a
invalidObject errs = Left $ InvalidObject errs

instance (NamedResource k, Show k, BasedToJSON v)
         => WebResult (CreateResult (Versioned k v)) where
    report (Right (Right (Right a))) =
        do status created201  -- http://tools.ietf.org/html/rfc2616#section-10.2.2
           url <- resourceURL $ thingid $ version a
           setHeader "Location" $ LT.pack $ show $ url
           json a
    report (Left (InvalidObject msgs)) =
        do status badRequest400
           text $ LT.concat [ LT.concat [msg, "\r\n"] | msg<-msgs ]
    report (Right gr) = report gr

{- |
    Make a result of the contents of a singleton list.
    Fails with 'notFound' if the list is empty;
    fails with 'failure' if the list has more than one element.
-}
one :: (Result r a) => [a] -> r
one [x] = success $ x
one [] = notFound
one _ = failure "multiple objects found!"

{- |
    Make a result of a nonempty list.
    Fails with 'notFound' if the list is empty.
-}
some :: (Result r [a]) => [a] -> r
some [] = notFound
some x = success x
