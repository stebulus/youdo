module YouDo.Test where
import Control.Applicative ((<$>))
import Data.List ((\\))
import Distribution.TestSuite

plainTest :: String -> IO Result -> Test
plainTest testName f = Test $ TestInstance
    { run = Finished <$> f
    , name = testName
    , tags = []
    , options = []
    , setOption = \_ _ -> Left "no options supported"
    }

(~=) :: (Eq a, Show a) => a -> a -> Result
x ~= y = if x == y then Pass else Fail $ (show x) ++ " /= " ++ (show y)

permutationOf :: (Eq a, Show a) => [a] -> [a] -> Result
xs `permutationOf` ys =
    case (xs \\ ys) ~= [] of
        Pass -> (ys \\ xs) ~= []
        r -> r
