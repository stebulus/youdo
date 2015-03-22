{-|
Module      : YouDo.Const
Description : Convenience functions for working with Control.Applicative.Const.
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3
-}
module YouDo.Const (
    addConst)
where

import Control.Applicative (
    Const(..))
import Data.Monoid (
    Monoid,
    (<>))

addConst :: (Monoid a) => Const a b -> a -> Const a b
addConst c a = Const $ getConst c <> a
