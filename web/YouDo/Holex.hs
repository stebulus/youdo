{-# LANGUAGE ExistentialQuantification #-}
module YouDo.Holex where
import Control.Applicative (Applicative(..))
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.List (foldl')
import Data.Monoid (Sum(..))
import Data.Text.Lazy (Text)
import Web.Scotty (Parsable(..))

-- An expression with named holes.
-- k is the type of the names; v is the type of the values that fill
-- holes; a is the type of the expression.
data (Eq k) => Holex k v e a
    = Const a
    | forall b. Apply (Holex k v e (b->a)) (Holex k v e b)
    | forall b. TryApply (Holex k v e (b->Either (HolexError k v e) a)) (Holex k v e b)
    | TryApplyFailed (HolexError k v e)
    | Hole k (v->a)

keys :: (Eq k) => Holex k v e a -> [k]
keys (Const _) = []
keys (Apply exprf exprx) = keys exprf ++ keys exprx
keys (TryApply exprf exprx) = keys exprf ++ keys exprx
keys (TryApplyFailed _) = []
keys (Hole k _) = [k]

errors :: (Eq k) => Holex k v e a -> [HolexError k v e]
errors (Const _) = []
errors (Apply exprf exprx) = errors exprf ++ errors exprx
errors (TryApply exprf exprx) = errors exprf ++ errors exprx
errors (TryApplyFailed e) = [e]
errors (Hole _ _) = []

hole :: (Eq k) => k -> Holex k v e v
hole k = Hole k id

check :: (Eq k, Show e) => (a->Bool) -> e -> Holex k v e a -> Holex k v e a
check good err expr =
    TryApply (Const (\x -> if good x then Right x else Left (CustomError err))) expr

parse :: (Eq k, Parsable a) => k -> Holex k Text e a
parse k = TryApply (Const (\x -> case parseParam x of
                                    Left err -> Left (ParseError k x err)
                                    Right val -> Right val))
                   $ hole k

instance (Eq k) => Functor (Holex k v e) where
    fmap f (Const x) = Const (f x)
    fmap f (Apply exprg exprx) = Apply (fmap (f.) exprg) exprx
    fmap f (TryApply exprg exprx) = TryApply (fmap ((fmap f) .) exprg) exprx
    fmap _ (TryApplyFailed err) = TryApplyFailed err
    fmap f (Hole k g) = Hole k (f.g)
instance (Eq k) => Applicative (Holex k v e) where
    pure = Const
    (Const f) <*> (Const x) = Const (f x)
    (Const f) <*> (Hole k g) = Hole k (f.g)
    (Hole k f) <*> (Const x) = Hole k (($x).f)
    u <*> v = Apply u v

data HolexError k v e = MissingKey k
                      | UnusedKey k
                      | DuplicateValue k v
                      | ParseError k v Text
                      | CustomError e
    deriving (Show, Eq)

runHolex :: (Eq k) => Holex k v e a -> [(k,v)] -> Either [HolexError k v e] a
runHolex expr kvs =
    case (value,allerrs) of
        (Const x,[]) -> Right x
        _ -> Left $ allerrs ++ (map MissingKey $ keys value)
                            ++ (errors value)
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

fill1 :: (Eq k) => Holex k v e a -> k -> v -> Writer (Sum Int) (Holex k v e a)
fill1 expr@(Const _) _ _ = return expr
fill1 (Apply exprf exprx) k v = do
    exprf' <- fill1 exprf k v
    exprx' <- fill1 exprx k v
    return $ exprf' <*> exprx'
fill1 (TryApply exprf exprx) k v = do
    exprf' <- fill1 exprf k v
    exprx' <- fill1 exprx k v
    return $ case (exprf',exprx') of
        (Const f,Const x) -> case (f x) of
                                Left err -> TryApplyFailed err
                                Right y -> Const y
        _ -> TryApply exprf' exprx'
fill1 expr@(TryApplyFailed _) _ _ = return expr
fill1 expr@(Hole key f) k v
    | key == k = do
        tell (Sum 1)
        return $ Const (f v)
    | otherwise = return expr
