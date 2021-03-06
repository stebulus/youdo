{-|
Module      : YouDo.Web.ActionM
Description : Tools for using, and tweaks of, Scotty's ActionM monad.
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.Web.ActionM (
    -- * Manipulating Scotty errors
    catchActionError,
    bindError,
    -- * Setting HTTP status in Scotty errors
    ActionStatusM,
    ErrorWithStatus,
    raiseStatus,
    badRequest,
    -- * Converting between @ActionM@ and @ActionStatusM@
    failWith,
    lift500,
    statusErrors)
where

import Control.Monad.Error (
    mapErrorT,
    throwError)
import Network.HTTP.Types (
    badRequest400,
    internalServerError500,
    Status(..))
import qualified Data.Text.Lazy as LT
import Web.Scotty (
    ActionM,
    status,
    text)
import Web.Scotty.Internal.Types (
    ScottyError(..),
    ActionError(..),
    ActionT(..))

-- | Perform the given action, catching any Scotty exception raised.
catchActionError :: (ScottyError e, Monad m)
    => ActionT e m a -> ActionT e' m (Either (ActionError e) a)
catchActionError act =
    ActionT $ mapErrorT
        (\mea -> do
            ea <- mea
            return $ Right ea)
        (runAM act)

-- | Monadically alter the exception of a Scotty state.
bindError :: (ScottyError e, ScottyError e', Monad m)
    => ActionT e m a -> (e -> ActionT e' m a) -> ActionT e' m a
bindError act f = do
    eith <- catchActionError act
    case eith of
        Right a -> return a
        Left (ActionError e) -> f e
        Left (Redirect msg) -> throwError (Redirect msg)
        Left Next -> throwError Next

-- | A version of 'ActionM' that includes HTTP 'Status' information in errors.
type ActionStatusM = ActionT ErrorWithStatus IO

-- | A 'ScottyError' type that includes HTTP 'Status' information.
data ErrorWithStatus = ErrorWithStatus Status LT.Text
instance ScottyError ErrorWithStatus where
    stringError msg = ErrorWithStatus internalServerError500 (LT.pack msg)
    showError (ErrorWithStatus _ msg) = msg

-- | Raise the indicated error.
raiseStatus :: Status -> LT.Text -> ActionStatusM a
raiseStatus stat msg = throwError $ ActionError $ ErrorWithStatus stat msg

-- | Equivalent to 'raiseStatus' 'badRequest400'
-- (See <http://tools.ietf.org/html/rfc2616#section-10.4.1>.)
badRequest :: LT.Text -> ActionStatusM a
badRequest = raiseStatus badRequest400

-- | Perform the given action, annotating any failures with the given
-- HTTP status.
failWith :: Status -> ActionM a -> ActionStatusM a
failWith stat act =
    ActionT $ mapErrorT
        (\m -> do
            eith <- m
            return $ case eith of
                Left (ActionError msg) ->
                    Left $ ActionError $ ErrorWithStatus stat msg
                Left Next -> Left Next
                Left (Redirect msg) -> Left $ Redirect msg
                Right x -> Right x)
        (runAM act)

-- | Equivalent to 'failWith' 'internalServerError500'.
-- (See <http://tools.ietf.org/html/rfc2616#section-10.5.1>.)
lift500 :: ActionM a -> ActionStatusM a
lift500 = failWith internalServerError500

-- | Report any error status to the web client.
statusErrors :: ActionStatusM () -> ActionM ()
statusErrors = (`bindError` reportStatus)
    where reportStatus (ErrorWithStatus stat msg) =
                do status stat
                   text msg
