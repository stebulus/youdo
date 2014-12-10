{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module YouDo.Holex where
import Control.Applicative ((<$>), Applicative(..))
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.Functor.Identity (Identity(..))
import Data.List (foldl')
import Data.Monoid (Sum(..), Monoid(..))
import Data.Text.Lazy (Text)

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

data HolexError k v = MissingKey k
                    | UnusedKey k
                    | DuplicateValue k v
                    | ParseError k v Text
                    | CustomError Text
    deriving (Show, Eq)

keys :: (Eq k) => Holex k v a -> [k]
keys expr = accumulate key expr
    where key (Hole k _) = Just [k]
          key _ = Nothing

errors :: (Eq k) => Holex k v a -> [HolexError k v]
errors expr = accumulate err expr
    where err (TryApplyFailed e) = Just [e]
          err _ = Nothing

hole :: (Eq k) => k -> Holex k v v
hole k = Hole k id

check :: (Eq k) => (a->Bool) -> Text -> Holex k v a -> Holex k v a
check good err expr =
    tryApply (Const (\x -> if good x then Right x else Left (CustomError err))) expr

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

setDefaults :: (Eq k) => Holex k v a -> Holex k v a
setDefaults expr = recursively deflt expr
    where deflt (Default v _) = Just $ Const v
          deflt _ = Nothing

runHolex :: (Eq k) => Holex k v a -> [(k,v)] -> Either [HolexError k v] a
runHolex expr kvs =
    case (valueWithDefaults, allErrs) of
        (Const x,[]) -> Right x
        _ -> Left allErrs
    where (value,_,someErrs) = foldl' kv1 (expr,[],[]) kvs
          valueWithDefaults = setDefaults value
          allErrs = someErrs
                    ++ (errors value)
                    ++ (map MissingKey $ keys valueWithDefaults)
                    -- (Find e.g. parse errors in value, not valueWithDefaults,
                    -- so that parse errors are not suppressed by defaulting;
                    -- but compute missing key errors from valueWithDefaults
                    -- so that absent keys with default values are not errors.)
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
fill1 expr k v = recursivelyM fill expr
    where fill (Hole key f)
            | key == k = do
                tell (Sum 1)
                return $ Just $ Const (f v)
            | otherwise = return Nothing
          fill _ = return Nothing

recursively :: (Eq k)
    => (forall b. Holex k v b -> Maybe (Holex k v b))
    -> Holex k v a
    -> Holex k v a
recursively f expr = runIdentity $ recursivelyM (return . f) expr

recursivelyM :: (Eq k, Monad m)
    => (forall b. Holex k v b -> m (Maybe (Holex k v b)))
    -> Holex k v a
    -> m (Holex k v a)
recursivelyM f expr = do
    result <- f expr
    case result of
        Just expr' ->
            return expr'
        Nothing ->
            case expr of
                Apply exprf exprx -> do
                    exprf' <- recursivelyM f exprf
                    exprx' <- recursivelyM f exprx
                    return $ exprf' <*> exprx'
                TryApply exprf exprx -> do
                    exprf' <- recursivelyM f exprf
                    exprx' <- recursivelyM f exprx
                    return $ tryApply exprf' exprx'
                Default v exprx -> do
                    exprx' <- recursivelyM f exprx
                    return $ defaultTo v exprx'
                _ -> return expr

accumulate :: (Eq k, Monoid n)
    => (forall b. Holex k v b -> Maybe n)
    -> Holex k v a
    -> n
accumulate f expr =
    case f expr of
        Just n -> n
        Nothing -> case expr of
                        Apply exprf exprx ->
                            accumulate f exprf `mappend` accumulate f exprx
                        TryApply exprf exprx ->
                            accumulate f exprf `mappend` accumulate f exprx
                        Default _ exprx ->
                            accumulate f exprx
                        _ -> mempty
