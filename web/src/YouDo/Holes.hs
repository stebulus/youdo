{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, FlexibleContexts #-}
module YouDo.Holes where

import Control.Applicative
import Control.Applicative.Lift
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Maybe
import Data.Monoid

class Holes k v f | f->k v where
    hole :: k -> f v

class Errs e f | f->e where
    check :: (a->Bool) -> e -> f a -> f a
    catch :: f a -> (e -> a) -> f a

(?:) :: (Errs e f) => f a -> a -> f a
fx ?: y = fx `catch` const y

optional :: (Functor f, Errs e f) => f a -> f (Maybe a)
optional fx = (Just <$> fx) ?: Nothing

evaluate :: ([(k,v)]->a) -> [(k,v)] -> a
evaluate = id
instance (Eq k) => Holes k v ((->) [(k,v)]) where
    hole k = fromJust . lookup k

evaluateE :: (Eq k)
    => (Compose ((->) [(k,v)]) (Errors [String]) a)
    -> [(k,v)] -> Either [String] a
evaluateE x kvs =
    case getCompose x kvs of
        Pure a -> Right a
        Other (Constant es) -> Left es
instance (Eq k, Monoid e, MissingKeyError k e)
         => Holes k v (Compose ((->) [(k,v)]) (Errors e)) where
    hole k = Compose $ \kvs ->
        case lookup k kvs of
            Nothing -> failure $ missingKeyError k
            Just v -> pure v
instance (Monoid e, MissingKeyError k e)
         => Errs e (Compose ((->) [(k,v)]) (Errors e)) where
    check good err x = Compose $ \kvs ->
        case getCompose x kvs of
            Other (Constant e) -> failure e
            Pure a -> if good a then pure a else failure err
    x `catch` handle = Compose $ \kvs ->
        case getCompose x kvs of
            Other (Constant e) -> pure $ handle e
            Pure a -> pure a
class MissingKeyError k e where
    missingKeyError :: k -> e
instance (Show k) => MissingKeyError k [String] where
    missingKeyError k = ["missing key " ++ show k]

names :: Phantom (v,e) (Constant [k]) a -> [k]
names = getConstant . dePhantom
-- Phantom is needed to satisfy the coverage condition.
instance Holes k v (Phantom (v,e) (Constant [k])) where
    hole k = Phantom $ Constant [k]
instance Errs e (Phantom (v,e) (Constant [k])) where
    check _ _ x = x
    x `catch` _ = x
newtype Phantom ph f a = Phantom { dePhantom :: f a }
instance (Applicative f) => Applicative (Phantom ph f) where
    pure x = Phantom $ pure x
    f <*> x = Phantom $ dePhantom f <*> dePhantom x
instance (Functor f) => Functor (Phantom ph f) where
    fmap f x = Phantom $ fmap f $ dePhantom x
