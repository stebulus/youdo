{-# LANGUAGE OverloadedStrings #-}
module YouDo.TimeParser where
import Control.Applicative ((<$>), (<*))
import Data.Attoparsec.Text.Lazy (digit, decimal, (<?>), endOfInput, satisfy,
    Parser, parse, Result(..))
import Data.Char (digitToInt)
import Data.List
import qualified Data.Text.Lazy as LT
import Data.Time (UTCTime(..), picosecondsToDiffTime, fromGregorianValid,
    Day, DiffTime)

{- |
    Attoparsec parser for @UTCTime@.  Accepts only the ISO-8601 format
    @YYYY-MM-DDThh:mm:ss.sssZ@, and accepts only valid dates.

    Note that @parseISO8601@ from "Data.Time.ISO8601" accepts input
    designating invalid dates such as @"2014-11-31T00:00:00.000Z"@;
    this is ultimately due to the use in the @ParseTime@
    instance for @Day@ (defined in "Data.Time.Format.Parse") of
    @fromGregorian@ instead of @fromGregorianValid@ (both defined in
    "Data.Time.Calendar").  We could fix this by defining our own
    instance of @ParseTime@ with the desired behaviour, but this
    would involve duplicating the whole stack of code above that,
    and it would require supporting all the @strftime()@-style format
    characters (onky a few of which are relevant to us).  So instead
    we just write a simple direct parser.
-}

parseUTCTime :: LT.Text -> Either String UTCTime
parseUTCTime t =
    case parse (utctime <* (endOfInput <?> "expected end of input")) t of
        Done "" result -> Right result
        Fail unconsumed stack msg -> Left $ intercalate " / " $
            stack ++ [msg, "next characters: " ++ (take 10 $ LT.unpack unconsumed)]
        _ -> error "impossible; endOfInput consumes everything"

-- The standard attoparsec 'char' has a less pleasant error message.
expect :: Char -> Parser ()
expect c = do
    _ <- satisfy (== c) <?> "expected " ++ (show c)
    return ()

utctime :: Parser UTCTime
utctime = (do
    d <- date
    expect 'T'
    t <- time
    return $ UTCTime d t) <?> "parsing UTC time"

date :: Parser Day
date = (p <?> "parsing date") >>= maybe (fail "no such date") return
    where p = do
            y <- fromIntegral <$> year
            expect '-'
            m <- month
            expect '-'
            d <- day
            return $ fromGregorianValid y m d

year :: Parser Int
year = decimal <?> "parsing year (YYYY)"

month :: Parser Int
month = between 1 12 decimal2 <?> "parsing month (MM)"

day :: Parser Int
day = between 1 31 decimal2 <?> "parsing day (DD)"

time :: Parser DiffTime
time = p <?> "parsing time"
    where p = do
            h <- hour
            expect ':'
            mi <- minute
            expect ':'
            s <- second
            expect '.'
            ss <- subsecond
            expect 'Z'
            if h == 24 && not (mi == 0 && s == 0 && ss == 0)
              then fail "hour 24 only allowed in 24:00:00.000Z"
              else do
                let milliseconds = fromIntegral $ ((h*60 + mi)*60 + s)*(1000) + ss
                -- we're just trusting the leap second if it's there  :(
                return $ picosecondsToDiffTime $ milliseconds*1000000000

hour :: Parser Int
hour = between 0 24 decimal2 <?> "parsing hour (hh)"  -- allow midnight at end of day

minute :: Parser Int
minute = between 0 59 decimal2 <?> "parsing minute (mm)"

second :: Parser Int
second = between 0 60 decimal2 <?> "parsing second (ss)"  -- allow leap second

subsecond :: Parser Int
subsecond = decimal3 <?> "parsing subsecond (sss)"

numeral :: Parser Int
numeral = digitToInt <$> digit <?> "expected digit"

(<:>) :: Parser Int -> Parser Int -> Parser Int
pa <:> pb = do
    a <- pa
    b <- pb
    return $ 10*a + b
infixl <:>

decimal2 :: Parser Int
decimal2 = numeral <:> numeral

decimal3 :: Parser Int
decimal3 = numeral <:> numeral <:> numeral

check :: (Monad m) => (a->Bool) -> (a->String) -> m a -> m a
check f msg ma = do
    a <- ma
    if f a
        then return a
        else fail $ msg a

between :: (Monad m, Ord a, Show a) => a -> a -> m a -> m a
between lo hi ma =
    check (\n -> lo <= n && n <= hi)
          (\n -> (show n) ++ " is not in range ["
                 ++ (show lo) ++ "," ++ (show hi) ++ "]")
          ma
