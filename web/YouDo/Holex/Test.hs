{-# LANGUAGE OverloadedStrings #-}
module YouDo.Holex.Test where
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Writer.Lazy (runWriter)
import Distribution.TestSuite
import Data.Monoid (Sum(..))
import YouDo.Holex
import YouDo.Test

tests :: IO [Test]
tests = return
    [ plainTest "evaluate Holex" $ do
        let expr :: Holex String Int Int
            expr = (+) <$> hole "a" <*> hole "b"
        runHolex expr [("a", 3), ("b", 2)] ~= Right 5
    , plainTest "Holex fill1" $ do
        let expr :: Holex String Int Int
            expr = (+) <$> hole "a" <*> ((*) <$> hole "b" <*> hole "a")
        map (getSum . snd . runWriter . (\k -> fill1 k 3 expr)) ["a","b","c"]
            ~= [2,1,0]
    , plainTest "Holex errors" $ do
        let expr :: Holex String Int Int
            expr = (+) <$> hole "a" <*> hole "b"
        case runHolex expr [("a", 3), ("a", 2), ("c", 4)] of
            Right n -> failure $ "evaluated to " ++ (show n)
            Left errs -> errs `permutationOf`
                         [ MissingKey "b"
                         , UnusedKey "c"
                         , DuplicateValue "a" 2
                         ]
    , plainTest "Holex testing errors" $ do
        let expr :: Holex String Int Int
            expr = (+) <$> (check (>0) "a must be positive" $ hole "a") <*> hole "b"
        runHolex expr [("a", 1), ("b", -3)] ~= Right (-2)
        runHolex expr [("a", -1), ("b", -3)] ~= Left [CustomError "a must be positive"]
    , plainTest "default values in Holexes" $ do
        let expr :: Holex String Int Int
            expr = (+) <$> (defaultTo 3 (hole "a")) <*> (hole "b")
        runHolex expr [("a", 1), ("b", -3)] ~= Right (-2)
        runHolex expr [("b", -3)] ~= Right 0
    , plainTest "default doesn't suppress validation errors" $ do
        let expr :: Holex String Int Int
            expr = (+) <$> (defaultTo 3 $ check (>0) "a must be positive" $ hole "a")
                       <*> (hole "b")
        runHolex expr [("a", -2), ("b", 3)] ~= Left [CustomError "a must be positive"]
    , plainTest "maybe-values in Holexes" $ do
        let expr :: Holex String Int (Int, Maybe Int)
            expr = (,) <$> hole "a" <*> optional (hole "b")
        runHolex expr [("a", 1), ("b", 2)] ~= Right (1, Just 2)
        runHolex expr [("a", 1)] ~= Right (1, Nothing)
    ]
