{-|
Module      : YouDo.Monad.Null
Description : A monad that does nothing.
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.Monad.Null (NullMonad(..)) where

data NullMonad a = NullMonad

instance Monad NullMonad where
    return _ = NullMonad
    _ >>= _ = NullMonad
