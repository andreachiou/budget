module Date ( Date (..)
            , day
            , plus
            , plus1Month
            , theDay
            , yearMonthDay)
    where

import Data.Time 
import Data.Csv ( FromField
                , ToField
                , parseField 
                , toField 
                )
import FieldToString
import Data.List.Split ( splitOn )
import Text.Printf

newtype Date = Date Day
    deriving (Eq, Ord)

day :: Date -> Day
day (Date d) = d

theDay :: Integer -> Int -> Int -> Date
theDay y m d = Date (fromGregorian y m d)

plus :: Date -> Integer -> Date
plus (Date d) n = Date (addDays n d)

plus1Month :: Date -> Date
plus1Month (Date d) = Date (addGregorianMonthsClip 1 d)

yearMonthDay :: Date -> (Integer, Int, Int)
yearMonthDay = toGregorian . day

instance Show Date where
    show (Date d) =  formatTime defaultTimeLocale "%m/%d/%Y" d

instance ToField Date where
    toField = stringToField . show

instance FromField Date where
    parseField = fmap Date . parseTimeM True defaultTimeLocale "%m/%d/%Y" . spaceToZero . fieldToString
        where 
            spaceToZero s = case splitOn "/" s of
                              [m,d,y] -> printf "%02d/%02d/%04d" 
                                (read m :: Int) 
                                (read d :: Int ) 
                                (normalYear (read y))
                              _ -> s
            normalYear :: Int -> Int
            normalYear y 
                | y > 100 = y
                | y < 50  = 2000 + y
                | otherwise = 1900 + y
