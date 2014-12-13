{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, FlexibleContexts, OverloadedStrings,
    ScopedTypeVariables #-}
module YouDo.DB where

import Control.Applicative
import Data.Aeson (FromJSON(..))
import qualified Data.Text.Lazy as LT
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Web.Scotty (Parsable(..))

-- d contains versioned key value pairs (k,v), in monad m
class (Monad m, NamedResource k)
      => DB k v u m d | d->v, d->k, d->u, d->m where
    get :: k -> d -> m (GetResult (Versioned k v))
    getVersion :: VersionedID k -> d -> m (GetResult (Versioned k v))
    getVersions :: k -> d -> m (GetResult [Versioned k v])
    getAll :: d -> m (GetResult [Versioned k v])
    post :: v -> d -> m (PostResult (Versioned k v))
    update :: u -> d -> m (UpdateResult (Versioned k v) (Versioned k v))
    dbResourceName :: d -> String
    dbResourceName = const $ resourceName x
        where x = Nothing :: Maybe k    -- ScopedTypeVariables used!

class Result r a | r->a where
    notFound :: r
    failure :: LT.Text -> r
    success :: a -> r

data NotFound = NotFound
type GetResult a = Either NotFound (Either LT.Text a)
instance Result (GetResult a) a where
    notFound = Left NotFound
    failure msg = Right $ Left msg
    success x = Right $ Right x

data NewerVersion a = NewerVersion a
type UpdateResult b a = Either (NewerVersion b) (GetResult a)
instance Result (UpdateResult b a) a where
    notFound = Right $ notFound
    failure = Right . failure
    success = Right . success
newerVersion :: b -> UpdateResult b a
newerVersion x = Left $ NewerVersion x

data InvalidObject = InvalidObject [LT.Text]
type PostResult a = Either InvalidObject (GetResult a)
instance Result (PostResult a) a where
    notFound = Right $ notFound
    failure = Right . failure
    success = Right . success
invalidObject :: [LT.Text] -> PostResult a
invalidObject errs = Left $ InvalidObject errs

one :: (Result r a) => [a] -> r
one [x] = success $ x
one [] = notFound
one _ = failure "multiple objects found!"

some :: (Result r [a]) => [a] -> r
some [] = notFound
some x = success x

data VersionedID a = VersionedID
    { thingid :: a
    , txnid :: TransactionID
    } deriving (Show, Eq)

data Versioned a b = Versioned
    { version :: VersionedID a
    , thing :: b
    } deriving (Show, Eq)

class NamedResource a where
    resourceName :: Maybe a -> String

newtype TransactionID = TransactionID Int deriving (Eq)
instance Show TransactionID where
    show (TransactionID n) = show n
instance FromField TransactionID where
    fromField fld = (fmap.fmap) TransactionID $ fromField fld
instance Parsable TransactionID where
    parseParam x = TransactionID <$> parseParam x
instance FromJSON TransactionID where
    parseJSON x = TransactionID <$> parseJSON x

