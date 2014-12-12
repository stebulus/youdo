{-# LANGUAGE ExistentialQuantification, RankNTypes, BangPatterns #-}
{- |
Module      : YouDo.Holex
Description : Expressions with named holes
Copyright   : (c) Steven Taschuk, 2014
License     : GPL-3

Example:

> (+) <$> hole "a" <*> hole "b"

is an expression with holes named @"a"@ and @"b"@.  Values for these
holes are supplied via 'runHolex':

> runHolex ((+) <$> hole "a" <*> hole "b") [("a", 1), ("b", 3)]

evaluates to @Right 4@.
-}
module YouDo.Holex (
    -- * Constructing
    hole, check, optional, defaultTo, tryApply,
    -- * Evaluating
    runHolex, HolexError(..),
    -- * Inspecting
    keys, errors, accumulate,
    -- * Altering
    fill1, recursively, recursivelyM,
    -- * The Holex type
    Holex(..)
) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Monad.Writer.Lazy (Writer, runWriter, tell)
import Data.Functor.Identity (Identity(..))
import Data.List (foldl')
import Data.Monoid (Sum(..), Monoid(..))
import Data.Text.Lazy (Text)

-- | An expression with named holes.  @k@ is the type of the names;
-- @v@ is the type of the values that fill holes; and @a@ is the
-- type of the entire expression.  The applicative instance reduces
-- applications of constant functions to constant arguments immediately
-- (that is, @Const f <*> Const x@ becomes a single @Const@ with a
-- thunk in it), but all other combinations are left as is.
data (Eq k) => Holex k v a
    = Const a   -- ^A constant expression.
    | forall b. Apply (Holex k v (b->a)) (Holex k v b)
                -- ^A function application.
    | forall b. TryApply (Holex k v (b->Either (HolexError k v) a)) (Holex k v b)
                -- ^An application of a function that may fail.
    | TryApplyFailed (HolexError k v)
                -- ^A 'TryApply' that /did/ fail.
    | Hole k (v->a)
                -- ^A hole with the given name and interpreter function.
    | Default a (Holex k v a)
                -- ^A default value if the subexpression cannot be evaluated.

instance (Eq k) => Functor (Holex k v) where
    fmap f expr = pure f <*> expr
instance (Eq k) => Applicative (Holex k v) where
    pure = Const
    Const f <*> Const x = Const (f x)
    u <*> v = Apply u v

-- | An error found by 'runHolex'.
data HolexError k v = MissingKey k          -- ^A hole named @k@ was not filled.
                    | UnusedKey k           -- ^A value for a hole named @k@ was given,
                                            -- but there is no such hole.
                    | DuplicateValue k v    -- ^Value @v@ was given for a hole named @k@,
                                            -- after another value was already given.
                    | ParseError k v Text   -- ^An error was detected by 'YouDo.Types.parse'.
                    | CustomError Text      -- ^An error was detected by 'check'.
    deriving (Show, Eq)

{- |
    A named hole.  For example,

    > runHolex (hole "a") [("a", 3)]  ==>  Right 3
-}
hole :: (Eq k)
        => k            -- ^The name of the hole.
        -> Holex k v v
hole k = Hole k id

{- |
    A required condition on a subexpression.  Under 'runHolex', either
    evaluates to the value of the subexpression (if the condition is
    satisfied) or posts a 'CustomError' with the given message (if not).
    For example,

    > runHolex (check (>0) "a is not positive" (hole "a")) [("a", 3)]
    >       ==>  Right 3
    > runHolex (check (>0) "a is not positive" (hole "a")) [("a", 0)]
    >       ==>  Left [CustomError "a is not positive"]
-}
check :: (Eq k)
         => (a->Bool)       -- ^The condition to satisfy.
         -> Text            -- ^An error message if the condition is not satisfied.
         -> Holex k v a     -- ^The subexpression.
         -> Holex k v a
check good err expr =
    tryApply (Const (\x -> if good x then Right x else Left (CustomError err))) expr

{- |
    An expression whose evaluation is optional.  Under 'runHolex',
    evaluates to @Just x@ if the subexpression evalutes to @x@,
    or to @Nothing@ if the subexpression cannot be evaluated (say,
    because an error occured in it or it contains unfilled holes).
    For example,

    > runHolex (optional (hole "a")) [("a", 3)]
    >       ==>  Right (Just 3)
    > runHolex (optional (hole "a")) []
    >       ==>  Right Nothing
-}
optional :: (Eq k) => Holex k v a -> Holex k v (Maybe a)
optional expr = defaultTo Nothing (Just <$> expr)

{- |
    Apply a function which might report failure.  Under 'runHolex',
    evaluates to @x@ if the function returns @Right x@, and posts an
    error otherwise.  For example,

    > f x = if even x then Right "even!" else Left (CustomError "nope")
    > runHolex (tryApply (Const f) (hole "a")) [("a", 3)]
    >       ==>  Left [CustomError "nope"]
    > runHolex (tryApply (Const f) (hole "a")) [("a", 4)]
    >       ==>  Right "even!"
-}
tryApply :: (Eq k) => (Holex k v (b->Either (HolexError k v) a)) -> Holex k v b
    -> Holex k v a
tryApply (Const f) (Const x) =
    case f x of
        Left err -> TryApplyFailed err
        Right b -> Const b
tryApply f expr = TryApply f expr

{- |
    A default value.  Under 'runHolex', evaluates to the value of
    the subexpression, or to the given default value if it cannot be
    evaluated (say, because an error occurred in it or it contains
    unfilled holes).
-}
defaultTo :: (Eq k) => a -> Holex k v a -> Holex k v a
defaultTo _ expr@(Const _) = expr
defaultTo v expr = Default v expr

{- |
    Evaluate the expression using the given values.  As much of the
    expression is evaluated as possible, and all errors encountered
    are reported.
-}
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
          kv1 (!e,!used,!errs) (k,v) =
                let (e',n) = runWriter $ fill1 k v e
                    noMatch = getSum n == 0
                    duplicate = k `elem` used
                    used' = if duplicate || noMatch
                            then used
                            else k:used
                    errs' = if duplicate
                            then (DuplicateValue k v):errs
                            else if noMatch
                            then (UnusedKey k):errs
                            else errs
                in (e',used',errs')

-- | Fill in holes having the given name; write the number of holes filled.
fill1 :: (Eq k) => k -> v -> Holex k v a -> Writer (Sum Int) (Holex k v a)
fill1 k v = recursivelyM fill
    where fill (Hole key f)
            | key == k = do
                tell (Sum 1)
                return $ Just $ Const (f v)
            | otherwise = return Nothing
          fill _ = return Nothing

-- | Replace any nodes created with `defaultTo` with their default
-- values, if they haven't already evaluated to something else.
setDefaults :: (Eq k) => Holex k v a -> Holex k v a
setDefaults expr = recursively deflt expr
    where deflt (Default v _) = Just $ Const v
          deflt _ = Nothing

{- |
    Recursively apply the given function to the Holex.  If the function
    returns Nothing, continue recursively to the children of the node
    if it has any, or keep it as-is if it has none.  If the function
    returns a Just, replace the current node with the contents of
    the Just; do not recurse into the new children.
-}
recursively :: (Eq k)
    => (forall b. Holex k v b -> Maybe (Holex k v b))
    -> Holex k v a
    -> Holex k v a
recursively f expr = runIdentity $ recursivelyM (return . f) expr

-- | Recursively apply the given function to the Holex, in a monad.
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

-- | The names of the (unfilled) holes in the given expression.
keys :: (Eq k) => Holex k v a -> [k]
keys expr = accumulate key expr
    where key (Hole k _) = Just [k]
          key _ = Nothing

-- | The errors from `tryApply` nodes that failed.
errors :: (Eq k) => Holex k v a -> [HolexError k v]
errors expr = accumulate err expr
    where err (TryApplyFailed e) = Just [e]
          err _ = Nothing

{- |
    Apply the given function to the nodes of the expression,
    accumulating the results.  If the function returns Nothing,
    recurse into the node's children.  If the function returns a Just,
    accumulate that value and do not recurse into the children.
-}
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
