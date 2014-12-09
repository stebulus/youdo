{-# LANGUAGE OverloadedStrings #-}
module YouDo.Holex.Test where
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Writer.Lazy (runWriter)
import Data.List ((\\))
import Distribution.TestSuite
import Data.Monoid (Sum(..))
import Data.Text.Lazy (Text)
import YouDo.Holex

tests :: IO [Test]
tests = return
    [ plainTest "evaluate Holex" $
        let expr :: Holex String Int Int
            expr = (+) <$> hole "a" <*> hole "b"
            result = case runHolex expr [("a", 3), ("b", 2)] of
                        Left errs -> Fail (show errs)
                        Right n -> n ~= 5
        in return result
    , plainTest "Holex fill1" $
        let expr :: Holex String Int Int
            expr = (+) <$> hole "a" <*> ((*) <$> hole "b" <*> hole "a")
            result = map (getSum . snd . runWriter . (\k -> fill1 expr k 3))
                         ["a","b","c"]
                     ~= [2,1,0]
        in return result
    , plainTest "Holex errors" $
        let expr :: Holex String Int Int
            expr = (+) <$> hole "a" <*> hole "b"
            result = case runHolex expr [("a", 3), ("a", 2), ("c", 4)] of
                        Right n -> Fail $ "evaluated to " ++ (show n)
                        Left errs -> errs `permutationOf`
                                     [ MissingKey "b"
                                     , UnusedKey "c"
                                     , DuplicateValue "a" 2
                                     ]
        in return result
    , plainTest "Holex testing errors" $
        let expr :: Holex String Int Int
            expr = (+) <$> (check (>0) "a must be positive" $ hole "a") <*> hole "b"
            val = runHolex expr [("a", 1), ("b", -3)]
            val' = runHolex expr [("a", -1), ("b", -3)]
            result = (val,val') ~= (Right (-2), Left [CustomError "a must be positive"])
        in return result
    , plainTest "Holex parsing" $
        let expr :: Holex String Text Int
            expr = (+) <$> (parse "a") <*> (parse "b")
            val = runHolex expr [("a", "1"), ("b", "-3")]
            val' = runHolex expr [("a", "1"), ("b", "q")]
            result = (val,val') ~= (Right (-2), Left [ParseError "b" "q" "readEither: no parse"])
        in return result
    , plainTest "default values in Holexes" $
        let expr :: Holex String Int Int
            expr = (+) <$> (defaultTo 3 (hole "a")) <*> (hole "b")
            val = runHolex expr [("a", 1), ("b", -3)]
            val' = runHolex expr [("b", -3)]
            result = (val,val') ~= (Right (-2), Right 0)
        in return result
    , plainTest "default doesn't suppress validation errors" $
        let expr :: Holex String Int Int
            expr = (+) <$> (defaultTo 3 $ check (>0) "a must be positive" $ hole "a")
                       <*> (hole "b")
            val = runHolex expr [("a", -2), ("b", 3)]
            result = val ~= Left [CustomError "a must be positive"]
        in return result
    , plainTest "maybe-values in Holexes" $
        let expr :: Holex String Int (Int, Maybe Int)
            expr = (,) <$> hole "a" <*> optional (hole "b")
            val = runHolex expr [("a", 1), ("b", 2)]
            val' = runHolex expr [("a", 1)]
            result = (val,val') ~= (Right (1,Just 2), Right (1,Nothing))
        in return result
    ]

(~=) :: (Eq a, Show a) => a -> a -> Result
x ~= y = if x == y then Pass else Fail $ (show x) ++ " /= " ++ (show y)

permutationOf :: (Eq a, Show a) => [a] -> [a] -> Result
xs `permutationOf` ys =
    case (xs \\ ys) ~= [] of
        Pass -> (ys \\ xs) ~= []
        r -> r

plainTest :: String -> IO Result -> Test
plainTest testName f = Test $ TestInstance
    { run = Finished <$> f
    , name = testName
    , tags = []
    , options = []
    , setOption = \_ _ -> Left "no options supported"
    }
