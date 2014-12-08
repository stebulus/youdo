{-# LANGUAGE ExistentialQuantification #-}
module YouDo.Holex where
import Control.Applicative (Applicative(..))
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.List (foldl')
import Data.Monoid (Sum(..))

-- An expression with named holes.
-- k is the type of the names; v is the type of the values that fill
-- holes; a is the type of the expression.
data (Eq k) => Holex k v a
    = Const a
    | forall b. Apply (Holex k v (b->a)) (Holex k v b)
    | Hole k (v->a)

keys :: (Eq k) => Holex k v a -> [k]
keys (Const _) = []
keys (Apply exprf exprx) = keys exprf ++ keys exprx
keys (Hole k _) = [k]

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

data HolexError k v = MissingKey k
                    | UnusedKey k
                    | DuplicateValue k v
    deriving (Show, Eq)

runHolex :: (Eq k) => Holex k v a -> [(k,v)] -> Either [HolexError k v] a
runHolex expr kvs =
    case (value,allerrs) of
        (Const x,[]) -> Right x
        _ -> Left $ allerrs ++ (map MissingKey $ keys value)
    where (value,_,allerrs) = foldl' kv1 (expr,[],[]) kvs
          kv1 (e,used,errs) (k,v) =
                let (e',n) = runWriter $ fill1 e k v
                    noMatch = getSum n == 0
                    duplicate = k `elem` used
                    used' = if not (duplicate || noMatch)
                            then k:used
                            else used
                    errs' = if duplicate
                            then (DuplicateValue k v):errs
                            else if noMatch
                            then (UnusedKey k):errs
                            else errs
                in (e',used',errs')

fill1 :: (Eq k) => Holex k v a -> k -> v -> Writer (Sum Int) (Holex k v a)
fill1 expr@(Const _) _ _ = return expr
fill1 (Apply exprf exprx) k v = do
    exprf' <- fill1 exprf k v
    exprx' <- fill1 exprx k v
    return $ exprf' <*> exprx'
fill1 expr@(Hole key f) k v
    | key == k = do
        tell (Sum 1)
        return $ Const (f v)
    | otherwise = return expr
