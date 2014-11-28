module YouDo.DB where
import Control.Applicative ((<$>), (<*>))
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

data Youdo = Youdo { id :: Maybe Int  -- Nothing for new Youdos
                   , assignerid :: Int
                   , assigneeid :: Int
                   , description :: String
                   , duedate :: Maybe UTCTime
                   , completed :: Bool
                   } deriving (Show)

instance FromRow Youdo where
    fromRow = Youdo <$> field <*> field <*> field <*> field <*> field <*> field

class DB a where
    getYoudo :: Int -> a -> IO [Youdo]
    postYoudo :: Youdo -> a -> IO Int
    getYoudos :: a -> IO [Youdo]
