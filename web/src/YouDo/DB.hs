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
    -- *Database
    DB(..), NamedResource(..), Updater(..),
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

import Control.Applicative ((<$>))
import Data.Aeson (FromJSON(..))
import qualified Data.Text.Lazy as LT
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Web.Scotty (Parsable(..))

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

    -- | Update an existing object.
    update :: (Versioned k u) -> d
        -> m (UpdateResult (Versioned k v) (Versioned k v))

    -- | The name of this resource.
    -- Obtained from the 'NamedResource' instance for 'k'.
    dbResourceName :: d -> String
    dbResourceName = const $ resourceName x
        where x = Nothing :: Maybe k    -- ScopedTypeVariables used!

-- | A @u@ represents a change to an @a@.
class Updater u a where
    doUpdate :: u -> a -> a

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

{- |
    An identifier of a version of a thing, as was produced in a
    particular transaction.
-}
data VersionedID a = VersionedID
    { thingid :: a
    , txnid :: TransactionID
    } deriving (Show, Eq)

-- | A version of a thing, as was produced in a particular transaction.
data Versioned a b = Versioned
    { version :: VersionedID a
    , thing :: b
    } deriving (Show, Eq)

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
