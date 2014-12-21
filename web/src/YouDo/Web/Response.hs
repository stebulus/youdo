{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : YouDo.Web.Response
Description : Responding to a web client
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.Web.Response (
    WebResult(..),
    augmentObject,
    BasedToJSON(..),
    basedjson)
where

import Data.Aeson (
    ToJSON(..),
    Value(..))
import Data.Aeson.Types (
    Pair)
import Data.HashMap.Strict (
    insert)
import Data.List (
    foldl')
import Network.URI (
    URI(..))
import Web.Scotty (
    ActionM,
    json)

import YouDo.Monad.Null

-- | A value that can be reported to a web client.
class WebResult m r where
    report :: URI                   -- ^The base URI.
           -> r                     -- ^The value to report.
           -> m ()                  -- ^An action that reports that value.

instance WebResult NullMonad r where
    report _ _ = NullMonad

-- | A value that can be serialized as JSON, respecting a base URI.
class BasedToJSON a where
    basedToJSON :: a -> URI -> Value
instance BasedToJSON a => BasedToJSON [a] where
    basedToJSON xs uri = toJSON $ map (flip basedToJSON uri) xs

-- | Like 'Scotty.json', but for based representations.
basedjson :: BasedToJSON a => a -> URI -> ActionM ()
basedjson x uri = json $ basedToJSON x uri

-- | Augment a JSON object with some extra pairs.
-- Causes a run-time error if the given object is not an 'Object'.
augmentObject :: Value -> [Pair] -> Value
augmentObject (Object m) pairs =
    Object $ foldl' (flip (uncurry insert)) m pairs
augmentObject _ _ = error "augmentObject: not an object"
