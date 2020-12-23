module YearlySelectionSpec
    where
import Test.Hspec
import YearlySelection
import Period

spec :: SpecWith ()
spec = do
    describe "yearly selection" $ do
        describe "absolute" $ do
            it "selects a year and the previous year, disregarding the given month" $ do
                yearlySelection 2020 1 Absolute `shouldBe`
                    YearlySelection Absolute
                        (Period (theDay 2020 1 1) (theDay 2020 12 31))
                        (Period (theDay 2019 1 1) (theDay 2019 12 31))

            it "selects any year and its previous year" $ do
                yearlySelection 2021 4 Absolute `shouldBe`
                    YearlySelection Absolute
                        (Period (theDay 2021 1 1) (theDay 2021 12 31))
                        (Period (theDay 2020 1 1) (theDay 2020 12 31))

        describe "running" $ do
            it "selects a year ending with the given month" $ do
                yearlySelection 2021 2 Running `shouldBe`
                    YearlySelection Running
                        (Period (theDay 2020 3 1) (theDay 2021 2 28))
                        (Period (theDay 2019 3 1) (theDay 2020 2 29))

        describe "toDate" $ do
            it "selects a period starting on jan 1st and ending with the given month and a the whole previous year" $ do
                yearlySelection 2021 4 ToDate `shouldBe`
                    YearlySelection ToDate
                        (Period (theDay 2021 1 1) (theDay 2021 4 30))
                        (Period (theDay 2020 1 1) (theDay 2020 12 31))

        describe "can be shown" $ do
            it "for an absolute selection" $ do
                show (yearlySelection 2020 1 Absolute) `shouldBe`
                    "Jan 2020-Dec 2020 | Jan 2019-Dec 2019"

            it "for a running selection" $ do
                show (yearlySelection 2020 4 Running) `shouldBe`
                    "2019-May-01 2020-Apr-30 | 2018-May-01 2019-Apr-30"

            it "for a to date selection" $ do
                show (yearlySelection 2020 4 ToDate) `shouldBe`
                    "2020-Jan-01 2020-Apr-30 | 2019-Jan-01 2019-Dec-31"




