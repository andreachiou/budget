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
        let t0  = simplified 2018 12 31 "Online Services" 1000.00
        let t1  = simplified 2019 04 01 "Online Services" 48.07
        let t2  = simplified 2019 02 02 "Online Services" 50.00
        let t3  = simplified 2020 01 01 "Online Services" 100.00
        let t4  = simplified 2020 07 12 "Online Services" 64.00
        let t5  = simplified 2021 01 01 "Online Services" 42.00
        let t6  = simplified 2018 12 31 "Car Insurance" 1000.00
        let t7  = simplified 2019 04 01 "Car Insurance" 96.14
        let t8  = simplified 2019 02 02 "Car Insurance" 100.00
        let t9  = simplified 2020 01 01 "Car Insurance" 200.00
        let t10 = simplified 2020 07 12 "Car Insurance" 128.00
        let t11 = simplified 2021 01 01 "Car Insurance" 42.00

        describe "lines" $ do
            it "show the total for a category for a given year and the previous year" $ do
                let transactions = [t0, t1, t2, t3, t4, t5]
                let report = yearlyLines (yearlySelection 2020 1 Absolute) allCategories transactions
                [take 80 (head report)] `shouldBeOutput` ["Online Services : 164.00 | 98.07"]

            it "show the total for many categories for a given year and the previous year" $ do
                let transactions = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11]
                let report = yearlyLines (yearlySelection 2020 1 Absolute) allCategories transactions
                [take 80 (head report)]        `shouldBeOutput` ["Car Insurance : 328.00 | 196.14"]
                [take 80 (head (tail report))] `shouldBeOutput` ["Online Services : 164.00 | 98.07"]

            it "show a total of zero for matching category with no transaction in the other year" $ do
                let transactions = [t0, t1, t2, t5, t6, t9, t10, t11]
                let report = yearlyLines (yearlySelection 2020 1 Absolute) allCategories transactions
                [take 80 (head report)]        `shouldBeOutput` ["Car Insurance : 328.00 | 0.00"]
                [take 80 (head (tail report))] `shouldBeOutput` ["Online Services : 0.00 | 98.07"]

        describe "yearly title" $ do
            describe "show a header for the given period, yearly time and category selection" $ do
                it "for an absolute year" $ do
                    yearlyTitle (yearlySelection 2020 1 Absolute) allCategories `shouldBeLine`
                        "Yearly report for all categories : Jan 2020-Dec 2020 | Jan 2019-Dec 2019" 

                it "for a running year" $ do
                    yearlyTitle (yearlySelection 2020 4 Running) allCategories `shouldBeLine`
                        "Yearly report for all categories : 2019-May-01 2020-Apr-30 | 2018-May-01 2019-Apr-30"
