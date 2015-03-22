{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : YouDo.Web.Service
Description : Tools for defining web services.
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.Web.Service (
    resource,
    API(..),
    setBase,
    toAssocList,
    toScotty)
where

import Data.List (
    intercalate)
import Data.Maybe (
    fromMaybe)
import Data.Monoid (
    Monoid(..))
import Data.String (
    fromString)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (
    methodNotAllowed405,
    StdMethod)
import Network.URI (
    URI(..),
    nullURI)
import Web.Scotty (
    addroute,
    matchAny,
    ScottyM,
    setHeader,
    status)

import YouDo.Web.ActionM
import YouDo.Web.Relative

{- |
    A web resource, with a complete list of its supported methods.
    Defining a resource this way causes a 405 (Method Not Allowed)
    response when a request uses a method which is not in the
    given list.  (Scotty's default is 404 (Not Found), which is less
    appropriate.)

    The URI argument to the enclosed functions is the base URI,
    whatever that means in the context of the application.
    (Note especially that it's not necessarily the URI of this
    resource itself; see 'API'.)
-}
resource :: [(StdMethod, URI -> a)]  -- ^Allowed methods and their actions.
            -> API a
resource acts = API [Based { base = Nothing
                           , relToBase = nullURI
                           , debased = acts
                           }]

{- |
    A web API.

    Create endpoints by calling 'resource' and adjust their location
    with '(//)'.  For example,

    @
        'u'"foo" // 'u'"bar" //
            'resource' [ (GET, getSomething)
                       , (POST, doSomething)
                       ]
    @

    produces a resource which supports HTTP GET and POST and has a
    nominal location of @"foo/bar"@.

    'API's may be combined; for example:

    @
        'u'"foo" // (
            'u'"bar" //
                'resource' [ (GET, getSomething)
                           , (POST, doSomething)
                           ]
            <>
            'u'"snee" // 'u' "qux" //
                'resource' [ (GET, getSomethingElse) ]
        )
    @

    produces an API with endpoints at @"foo/bar"@ and at
    @"foo/snee/qux"@.

    The functions passed to 'resource' (@getSomething@ and such
    above) receive a base URI as an argument.  By default, this is
    'nullURI'; it can be set to another value by calling 'setBase'
    at the appropriate point in a chain of '(//)'.  For example, with

    @
        'u'"foo" // setBase (
            'u'"bar" //
                'resource' [ (GET, getSomething)
                           , (POST, doSomething)
                           ]
            <>
            'u'"snee" // 'u' "qux" //
                'resource' [ (GET, getSomethingElse) ]
        )
    @

    the functions for both resources would receive the URI @"foo/"@
    as their base URI.  (There's a slash at the end because '(//)'
    was used instead of '(.//)'.)

    If 'setBase' is called more than once above a resource, the
    lowermost counts.  Thus you can set a high-level default base
    URI for most resources and override it for specific subtrees.

    The functions passed to 'resource' return type 'a'; ultimately this
    should be 'ActionStatusM ()' so that the 'API' can be passed to
    'toScotty', but other types may be useful at earlier stages of
    the computation.  (See 'Youdo.WebApp.api0', for example.)
-}
newtype API a = API [Based [(StdMethod, URI -> a)]]
instance RelativeToURI (API a) where
    uri .// (API xs) = API $ map (uri .//) xs
instance Monoid (API a) where
    mempty = API mempty
    (API xs) `mappend` (API ys) = API (xs `mappend` ys)
instance Functor API where
    fmap f (API xs) = API $ (fmap.fmap.fmap.fmap.fmap) f xs

-- | Set the base URI to the current point.  See 'API'.
setBase :: API a -> API a
setBase (API xs) = API $ map f xs
    where f x = x { base = Just $ maybe nullURI id (base x) }

-- | Convert an 'API' to a nested associative list.
toAssocList :: API a -> [(URI, [(StdMethod, a)])]
toAssocList (API xs) = map f xs
    where f based =
            let baseuri = fromMaybe nullURI $ base based
                uri = baseuri .// relToBase based
            in (uri, [ (method, actf baseuri) | (method, actf)<-debased based ])

-- | Convert an 'API' to a Scotty application.
toScotty :: API (ActionStatusM ()) -> ScottyM ()
toScotty api = mapM_ f $ toAssocList api
    where f (uri, acts) =
            let route = fromString $ uriPath uri
                allowedMethods = intercalate "," $ map (show . fst) acts
            in do
                sequence_ [ addroute method route
                            $ statusErrors
                            $ act
                          | (method, act)<-acts ]
                matchAny route $ do
                    status methodNotAllowed405
                        -- http://tools.ietf.org/html/rfc2616#section-10.4.6
                    setHeader "Allow" $ LT.pack allowedMethods
