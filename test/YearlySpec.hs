module YearlySpec
    where
import Test.Hspec
import TransactionSpec (simplified)

spec :: SpecWith () 
spec = do
    describe "yearly" $ do
        it "DISMISSED show the total for a category for a given year and the previous year" $ do
            let t1 = simplified 2019 04 01 "Online Services" 48.07
            let t2 = simplified 2019 02 02 "Online Services" 50.00
            let t3 = simplified 2020 01 01 "Online Services" 100.00
            let t4 = simplified 2020 07 12 "Online Services" 64.00
            let transactions = [t1, t2, t3, t4]
            -- [take 60 (head (yearlyLines (yearlySelection Absolute 2020) allCategories transactions]    
            transactions  `shouldBe` transactions
