{-# LANGUAGE FlexibleContexts #-}
module YouDo.Holes.Test where
import Control.Applicative
import Distribution.TestSuite
import YouDo.Test
import YouDo.Holes

expr :: (Applicative f, Holes String Int f, Errs [String] f)
     => f Int
expr = (+) <$> hole "a"
           <*> ((+) <$> check (>0) (pure ["not an error"]) (hole "b") ?: 0
                    <*> check (>0) (pure ["c should be positive"]) (hole "c"))

tests :: IO [Test]
tests = return
    [ plainTest "successful evaluation" $
        evaluateE expr [("a",1), ("b",2), ("c",3)] ~= Right 6
    , plainTest "multiple errors" $
        evaluateE expr [("b",-3), ("c",-3)]
            ~= Left ["missing key \"a\"", "c should be positive"]
    , plainTest "default value" $
        evaluateE expr [("a",1), ("b",-2), ("c",3)] ~= Right 4
    , plainTest "names" $
        names expr ~= ["a","b","c"]
    ]
