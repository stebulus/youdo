module YouDo.Test where
import Control.Monad.Trans.Either (EitherT(..), left, right)
import Data.List ((\\))
import Distribution.TestSuite

plainTest :: String -> TestResult -> Test
plainTest testName f = Test $ TestInstance
    { run = do
        result <- runEitherT f
        return $ Finished $ case result of
            Left err -> Fail err
            Right _ -> Pass
    , name = testName
    , tags = []
    , options = []
    , setOption = \_ _ -> Left "no options supported"
    }

permutationOf :: (Eq a, Show a) => [a] -> [a] -> TestResult
xs `permutationOf` ys = (xs \\ ys, ys \\ xs) ~= ([], [])

type TestResult = EitherT String IO ()
failure :: String -> TestResult
failure = left
success :: TestResult
success = right ()

-- Assert equality.
(~=) :: (Eq a, Show a) => a -> a -> TestResult
x ~= y = if x == y
         then success
         else failure $ "expected " ++ (show y) ++ ", got " ++ (show x)
infix 1 ~=
