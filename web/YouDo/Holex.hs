{-# LANGUAGE ExistentialQuantification #-}
module YouDo.Holex where
import Control.Applicative ((<$>), Applicative(..))
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.List (foldl')
import Data.Monoid (Sum(..))
import Data.Text.Lazy (Text)
import Web.Scotty (Parsable(..))

-- An expression with named holes.
-- k is the type of the names; v is the type of the values that fill
-- holes; a is the type of the expression.
data (Eq k) => Holex k v a
    = Const a
    | forall b. Apply (Holex k v (b->a)) (Holex k v b)
    | forall b. TryApply (Holex k v (b->Either (HolexError k v) a)) (Holex k v b)
    | TryApplyFailed (HolexError k v)
    | Hole k (v->a)
    | Default a (Holex k v a)

keys :: (Eq k) => Holex k v a -> [k]
keys (Const _) = []
keys (Apply exprf exprx) = keys exprf ++ keys exprx
keys (TryApply exprf exprx) = keys exprf ++ keys exprx
keys (TryApplyFailed _) = []
keys (Hole k _) = [k]
keys (Default _ expr) = keys expr

errors :: (Eq k) => Holex k v a -> [HolexError k v]
errors (Const _) = []
errors (Apply exprf exprx) = errors exprf ++ errors exprx
errors (TryApply exprf exprx) = errors exprf ++ errors exprx
errors (TryApplyFailed e) = [e]
errors (Hole _ _) = []
errors (Default _ _) = []

hole :: (Eq k) => k -> Holex k v v
hole k = Hole k id

check :: (Eq k) => (a->Bool) -> Text -> Holex k v a -> Holex k v a
check good err expr =
    tryApply (Const (\x -> if good x then Right x else Left (CustomError err))) expr

parse :: (Eq k, Parsable a) => k -> Holex k Text a
parse k = tryApply (Const (\x -> case parseParam x of
                                    Left err -> Left (ParseError k x err)
                                    Right val -> Right val))
                   $ hole k

optional :: (Eq k) => Holex k v a -> Holex k v (Maybe a)
optional expr = defaultTo Nothing (Just <$> expr)

tryApply :: (Eq k) => (Holex k v (b->Either (HolexError k v) a)) -> Holex k v b
    -> Holex k v a
tryApply (Const f) (Const x) =
    case f x of
        Left err -> TryApplyFailed err
        Right b -> Const b
tryApply f expr = TryApply f expr

defaultTo :: (Eq k) => a -> Holex k v a -> Holex k v a
defaultTo _ expr@(Const _) = expr
defaultTo v expr = Default v expr

instance (Eq k) => Functor (Holex k v) where
    fmap f expr = pure f <*> expr
instance (Eq k) => Applicative (Holex k v) where
    pure = Const
    Const f <*> Const x = Const (f x)
    u <*> v = Apply u v

data HolexError k v = MissingKey k
                    | UnusedKey k
                    | DuplicateValue k v
                    | ParseError k v Text
                    | CustomError Text
    deriving (Show, Eq)

setDefaults :: (Eq k) => Holex k v a -> Holex k v a
setDefaults (Apply exprf exprx) = setDefaults exprf <*> setDefaults exprx
setDefaults (TryApply exprf exprx) = tryApply (setDefaults exprf) (setDefaults exprx)
setDefaults (Default v _) = Const v
setDefaults expr = expr

runHolex :: (Eq k) => Holex k v a -> [(k,v)] -> Either [HolexError k v] a
runHolex expr kvs =
    case (setDefaults value, allerrs) of
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

fill1 :: (Eq k) => Holex k v a -> k -> v -> Writer (Sum Int) (Holex k v a)
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
fill1 (Default v' expr) k v = do
    expr' <- fill1 expr k v
    return $ defaultTo v' expr'
