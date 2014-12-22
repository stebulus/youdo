{-|
Module      : YouDo.Web.Relative
Description : Tools for respecting relative and base URIs.
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.Web.Relative (
    Based(..),
    RelativeToURI(..),
    (//),
    u)
where

import Network.URI (
    URI(..),
    nullURI,
    relativeTo)

-- | A wrapper that manages the URI of an object and its base URI.  (Used by 'API'.)
data Based a = Based { base :: Maybe URI
                     , relToBase :: URI
                     , debased :: a
                     }
instance RelativeToURI (Based a) where
    uri .// x@Based { base = Nothing } = x { relToBase = uri .// (relToBase x) }
    uri .// x@Based { base = Just y } = x { base = Just (uri .// y) }
instance Functor Based where
    fmap f based = based { debased = f $ debased based }

-- | Something that can be evaluated relative to a URI.
-- (Compare '(.//)' to '(//)'.)
class RelativeToURI a where
    (.//) :: URI -> a -> a
instance RelativeToURI URI where
    a .// b = b `relativeTo` a
infixl 7 .//

{- |
    This version of '(.//)' ensures that there is a slash
    at the end of the path part of the URI before evaluating 'b'
    relative to it. Thus, for example,

    @
        u "foo" // u "bar" == u "foo/bar"
    @

    which is probably what the writer intended.
-}
(//) :: (RelativeToURI b) => URI -> b -> b
a // b = a' .// b
    where a' = if maybeLast apath == Just '/'
               then a
               else a { uriPath = apath ++ "/" }
          apath = uriPath a
          maybeLast [] = Nothing
          maybeLast xs = Just $ last xs
infixl 7 //

-- | Create a 'URI' with the given string as path component.
-- (It's probably best to use this only for string literals
-- whose validity you can see yourself.)
u :: String -> URI
u s = nullURI { uriPath = s }
