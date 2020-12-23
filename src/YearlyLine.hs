module YearlyLine ( YearlyLine (..) )
    where

import Amount
import Category
import Text.Printf

data YearlyLine = YearlyLine { yearlyLineCategory :: Category
                             , yearlyLineCurrentAmount :: Amount
                             , yearlyLinePreviousAmount :: Amount 
                             }
                             deriving (Eq, Ord)

instance Show YearlyLine
    where
        show (YearlyLine cat currAmt prevAmt) =
            printf "%-49s:%10s |%10s"
                (categoryName cat) (show currAmt) (show prevAmt)
