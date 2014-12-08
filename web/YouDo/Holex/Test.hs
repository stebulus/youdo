module YouDo.Holex.Test where
import Control.Applicative ((<$>), (<*>))
import Distribution.TestSuite
import YouDo.Holex

tests :: IO [Test]
tests = return
    [ plainTest "evaluate Holex" $
        let expr :: Holex String Int Int
            expr = (+) <$> Hole "a" id
                       <*> Hole "b" id
            result = case runHolex expr [("a", 3), ("b", 2)] of
                        Left err -> Fail err
                        Right n | n == 5 -> Pass
                                | otherwise -> Fail $ (show n) ++ "/= 5"
        in return result
    ]

plainTest :: String -> IO Result -> Test
plainTest testName f = Test $ TestInstance
    { run = Finished <$> f
    , name = testName
    , tags = []
    , options = []
    , setOption = \_ _ -> Left "no options supported"
    }
