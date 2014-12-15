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
    throwLeft :: (Errs e f) => f (Either e a) -> f a
    catch :: f a -> (e -> f a) -> f a

throw :: (Functor f, Errs e f) => f e -> f a
throw fe = throwLeft (Left <$> fe)

throwIf :: (Applicative f, Errs e f) => f e -> f Bool -> f ()
throwIf fe fbool = throwLeft $
    (\e b -> if b then Left e else Right ()) <$> fe <*> fbool

check :: (Errs e f, Applicative f)
      => (a->Bool) -> f e -> f a -> f a
check good ferr x = throwLeft $ annotate <$> ferr <*> x
    where annotate e a = if good a then Right a else Left e

(?:) :: (Applicative f, Errs e f) => f a -> a -> f a
fx ?: y = fx `catch` const (pure y)

suppressError :: (Applicative f, Errs e f) => f a -> f (Maybe a)
suppressError fx = (Just <$> fx) ?: Nothing

type Evaluator k v = (->) [(k,v)]
evaluate :: Evaluator k v a -> [(k,v)] -> a
evaluate = id
instance (Eq k) => Holes k v ((->) [(k,v)]) where
    hole k = fromJust . lookup k

type EvaluatorE k v e = Compose ((->) [(k,v)]) (Errors e)
evaluateE :: (Eq k, Monoid e) => EvaluatorE k v e a -> [(k,v)] -> Either e a
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
    throwLeft fe = Compose $ \kvs ->
        case getCompose fe kvs of
            Pure (Left e) -> Other (Constant e)
            Pure (Right a) -> Pure a
            Other (Constant e') -> Other (Constant e')
    x `catch` handle = Compose $ \kvs ->
        case getCompose x kvs of
            Other (Constant e) ->
                getCompose (handle e) kvs
            Pure a -> pure a
class MissingKeyError k e where
    missingKeyError :: k -> e
instance (Show k) => MissingKeyError k [String] where
    missingKeyError k = ["missing key " ++ show k]

type NamesOnly k v e = Phantom (v,e) (Constant [k])
names :: NamesOnly k v e a -> [k]
names = getConstant . dePhantom
-- Phantom is needed to satisfy the coverage condition.
instance Holes k v (Phantom (v,e) (Constant [k])) where
    hole k = Phantom $ Constant [k]
instance Errs e (Phantom (v,e) (Constant [k])) where
    throwLeft fe = Phantom $ Constant $ getConstant $ dePhantom fe
    x `catch` _ = x
newtype Phantom ph f a = Phantom { dePhantom :: f a }
instance (Applicative f) => Applicative (Phantom ph f) where
    pure x = Phantom $ pure x
    f <*> x = Phantom $ dePhantom f <*> dePhantom x
instance (Functor f) => Functor (Phantom ph f) where
    fmap f x = Phantom $ fmap f $ dePhantom x
