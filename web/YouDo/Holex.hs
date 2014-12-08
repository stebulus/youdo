{-# LANGUAGE ExistentialQuantification #-}
module YouDo.Holex where
import Control.Applicative (Applicative(..))

-- An expression with named holes.
-- k is the type of the names; v is the type of the values that fill
-- holes; a is the type of the expression.
data (Eq k) => Holex k v a
    = Const a
    | forall b. Apply (Holex k v (b->a)) (Holex k v b)
    | Hole k (v->a)

instance (Eq k) => Functor (Holex k v) where
    fmap f (Const x) = Const (f x)
    fmap f (Apply exprg exprx) = Apply (fmap (f.) exprg) exprx
    fmap f (Hole k g) = Hole k (f.g)
instance (Eq k) => Applicative (Holex k v) where
    pure = Const
    (Const f) <*> (Const x) = Const (f x)
    (Const f) <*> (Hole k g) = Hole k (f.g)
    (Hole k f) <*> (Const x) = Hole k (($x).f)
    u <*> v = Apply u v

runHolex :: (Eq k) => Holex k v a -> [(k,v)] -> Either String a
runHolex expr kvs =
    case fill expr kvs of
        Const x -> Right x
        _ -> Left "incomplete evaluation"

fill :: (Eq k) => Holex k v a -> [(k,v)] -> Holex k v a
fill expr [] = expr
fill expr@(Const _) _ = expr
fill (Apply exprf exprx) kvs = fill exprf kvs <*> fill exprx kvs
fill expr@(Hole k f) kvs =
    case lookup k kvs of
        Nothing -> expr
        Just v -> Const (f v)
