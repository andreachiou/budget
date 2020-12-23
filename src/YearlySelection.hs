module YearlySelection ( YearlySelectionType (..)
                       , YearlySelection (..)
                       , yearlySelection)
    where
import Text.Printf
import Period                (Period (..), periodFromMonth, periodFromYear, theDay)
import Date                  (day, plus1Month)
import Data.Time.Format

data YearlySelectionType = Absolute | Running | ToDate
    deriving (Eq, Show)

data YearlySelection = YearlySelection YearlySelectionType Period Period
    deriving (Eq)

instance Show YearlySelection
    where
        show = showYearlySelection


showYearlySelection :: YearlySelection -> String
showYearlySelection (YearlySelection Absolute p1 p2) = printf "%s-%s | %s-%s" sd1 sd2 sd3 sd4
    where
        sd1 = fdate d1
        sd2 = fdate d2
        sd3 = fdate d3
        sd4 = fdate d4
        fdate = formatTime defaultTimeLocale "%b %Y" . day
        (Period d1 d2) = p1
        (Period d3 d4) = p2

showYearlySelection (YearlySelection Running p1 p2) = printf "%s %s | %s %s" sd1 sd2 sd3 sd4
    where
        sd1 = fdate d1
        sd2 = fdate d2
        sd3 = fdate d3
        sd4 = fdate d4
        fdate = formatTime defaultTimeLocale "%Y-%b-%d" . day
        (Period d1 d2) = p1
        (Period d3 d4) = p2

showYearlySelection (YearlySelection ToDate p1 p2) = printf "%s %s | %s %s" sd1 sd2 sd3 sd4
    where
        sd1 = fdate $ d1
        sd2 = fdate d2
        sd3 = fdate $ d3
        sd4 = fdate d4
        fdate = formatTime defaultTimeLocale "%Y-%b-%d" . day
        (Period d1 d2) = p1
        (Period d3 d4) = p2

yearlySelection :: Integer -> Int -> YearlySelectionType -> YearlySelection
yearlySelection y _ Absolute =
                    YearlySelection Absolute (periodFromYear y) (periodFromYear (y-1))

yearlySelection y m Running =
                    YearlySelection Running (Period (plus1Month d1) d2) (Period (plus1Month d3) d4)
                        where
                            (Period d1 _) = periodFromMonth (y-1) m
                            (Period _ d2) = periodFromMonth y m
                            (Period d3 _) = periodFromMonth (y-2) m
                            (Period _ d4) = periodFromMonth (y-1) m
yearlySelection y m ToDate =
                    YearlySelection ToDate (Period (theDay y 1 1) d2) (periodFromYear (y-1))
                        where
                            (Period _ d2) = periodFromMonth y m
