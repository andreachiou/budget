module YearlySelection ( YearlySelectionType (..)
                       , YearlySelection (..)
                       , yearlySelection)
    where
import Period                (Period (..), periodFromMonth, periodFromYear, theDay)

data YearlySelectionType = Absolute | Running | ToDate
    deriving (Eq, Show)

data YearlySelection = YearlySelection Period Period
    deriving (Eq, Show)

yearlySelection :: Integer -> Int -> YearlySelectionType -> YearlySelection
yearlySelection y _ Absolute =
                    YearlySelection (periodFromYear y) (periodFromYear (y-1))

yearlySelection y m Running =
                    YearlySelection (Period d1 d2) (Period d3 d4)
                        where
                            (Period d1 _) = periodFromMonth (y-1) m
                            (Period _ d2) = periodFromMonth y m
                            (Period d3 _) = periodFromMonth (y-2) m
                            (Period _ d4) = periodFromMonth (y-1) m
yearlySelection y m ToDate =
                    YearlySelection (Period (theDay y 1 1) d2) (periodFromYear (y-1))
                        where
                            (Period _ d2) = periodFromMonth y m
