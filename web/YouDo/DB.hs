module YouDo.DB where
import Data.Time (UTCTime)

data Youdo = Youdo { id :: Maybe Int  -- Nothing for new Youdos
                   , assignerid :: Int
                   , assigneeid :: Int
                   , description :: String
                   , duedate :: Maybe UTCTime
                   , completed :: Bool
                   } deriving (Show)

class DB a where
    getYoudo :: Int -> a -> IO [Youdo]
    postYoudo :: Youdo -> a -> IO Int
    getYoudos :: a -> IO [Youdo]
