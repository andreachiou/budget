module CurrentDay (currentDay
                  ,currentMonth)
    where
import Period
import Date
import Data.Time.Clock
import Data.Time.Calendar

currentDay :: IO (Integer, Int, Int)
currentDay = do
    now <- getCurrentTime
    return (toGregorian $ utctDay now)

currentMonth :: Maybe Period -> IO (Integer, Int)
currentMonth Nothing = do
                (y, m, _) <- currentDay
                return (y,m)
currentMonth (Just (Period _ end)) = do
        let (y, m, _) = yearMonthDay end
        return (y,m)
