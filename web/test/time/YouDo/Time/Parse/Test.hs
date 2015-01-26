{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE NamedFieldPuns #-}

module YouDo.Time.Parse.Test where

import Control.Applicative
import qualified Data.Text.Lazy as LT
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Data.Time.Clock
import Data.Time.Format
import Distribution.TestSuite
import System.Locale (defaultTimeLocale)
import Test.QuickCheck
import Text.Printf
import qualified Test.QuickCheck as Q
import qualified Distribution.TestSuite as TS

import YouDo.Time.Parse

-- support for quick check without cabal-test-quickcheck
toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {} = TS.Pass
toTSResult Q.GaveUp {} = TS.Fail "GaveUp"
toTSResult Q.Failure {Q.reason} = TS.Fail reason
toTSResult Q.NoExpectedFailure {} = TS.Fail "NoExpectedFailure"

runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
  qres <- Q.quickCheckWithResult Q.stdArgs {Q.maxSuccess = 30, 
  Q.maxSize = 20} prop
  return $ (Finished . toTSResult) qres

setOpt :: String -> String -> Either String TestInstance
setOpt _ _ = Left ""

tests :: IO [Test]
tests = return
    [ Test $ TestInstance (runQuickCheck $ \t ->
        Right t === parseUTCTime (formatUTCTime t)
      ) "round trip: format then parse" [] [] setOpt
    , Test $ TestInstance (runQuickCheck $
        forAll utcTimeString $ \s ->
            Right s === (formatUTCTime <$> parseUTCTime s)
      ) "round trip: parse then format" [] [] setOpt
    , Test $ TestInstance (runQuickCheck $
        forAll (choose (1859, 3600) :: Gen Integer) $ \year ->
            counterexample ";" $
            forAll (choose (1, 12) :: Gen Int) $ \month ->
                counterexample ";" $
                forAll (choose (1, 5) :: Gen Int) $ \tweak ->
                    counterexample ";" $
                    let maxday = monthLength (isLeapYear year) month
                        s = printf "%d-%02d-%02dT12:34:56.789Z"
                            year month (maxday + tweak)
                    in counterexample s $ isLeft
                       $ parseUTCTime $ LT.pack s
      ) "invalid dates rejected (a bit too big)" [] [] setOpt
    , Test $ TestInstance (runQuickCheck $
        forAll (choose (1859, 3600) :: Gen Integer) $ \year ->
            counterexample ";" $
            forAll (choose (1, 12) :: Gen Int) $ \month ->
                counterexample ";" $
                let s = printf "%d-%02d-00T12:34:56.789Z" year month
                in counterexample s $ isLeft
                   $ parseUTCTime $ LT.pack s
      ) "invalid dates rejected (zero)" [] [] setOpt
    ]

formatUTCTime :: UTCTime -> LT.Text
formatUTCTime t = LT.pack $
    reverse (drop 9 $ reverse
    $ formatTime defaultTimeLocale "%FT%T.%q" t)
    ++ "Z"

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary
    shrink = const []
instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> arbitrary
    shrink = const []
instance Arbitrary DiffTime where
    arbitrary = secondsToDiffTime <$> choose (0,86400)
        -- 86400 is legit because leap seconds; see UTCTime def
    shrink = const []

utcTimeString :: Gen LT.Text
utcTimeString = formatUTCTime <$> arbitrary

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True
