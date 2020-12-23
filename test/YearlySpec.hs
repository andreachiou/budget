module YearlySpec
    where
import Test.Hspec
import ShouldBeOutput 
import TransactionSpec (simplified)
import CategorySelection (allCategories)
import YearlySelection ( YearlySelectionType (..)
                       , yearlySelection )
import Yearly

spec :: SpecWith () 
spec = do
    describe "yearly" $ do
        it "DISMISSED show the total for a category for a given year and the previous year" $ do
            let t1 = simplified 2019 04 01 "Online Services" 48.07
            let t2 = simplified 2019 02 02 "Online Services" 50.00
            let t3 = simplified 2020 01 01 "Online Services" 100.00
            let t4 = simplified 2020 07 12 "Online Services" 64.00
            let transactions = [t1, t2, t3, t4]
            let report = yearlyLines (yearlySelection 2020 1 Absolute) allCategories transactions
            [take 80 (head report)] `shouldBeOutput` ["Online Services : 98.07 | 164.00"]
