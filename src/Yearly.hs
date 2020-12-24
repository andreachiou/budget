module Yearly ( yearly
              , yearlyFooter
              , yearlyLines
              , yearlyTitle)
    where

import Amount
import Category
import Period
import Transaction          (Transaction (..))
import TransactionList      (fromPeriod, groupByCategory, totalTransactions)
import YearlyLine ( YearlyLine (..))
import YearlySelection      (YearlySelection (..))
import CategorySelection    (CategorySelector)
import Text.Printf

type CategoryTotal = (Category,Amount)

yearly :: YearlySelection -> CategorySelector -> [Transaction] -> [String]
yearly ys catSel ts = [yearlyTitle ys catSel]
                    ++ yearlyLines ys catSel ts 
                    ++ [yearlyFooter ys catSel ts]

yearlyLines :: YearlySelection -> CategorySelector -> [Transaction] -> [String]
yearlyLines (YearlySelection _ cy py) _ ts = map show $ categoryTotals cy py ts

categoryTotals :: Period -> Period -> [Transaction] -> [YearlyLine]
categoryTotals cy py ts = zipYearlyGroups cgs pgs
    where
        cgs = (map categoryTotal . groupByCategory . fromPeriod cy) ts
        pgs = (map categoryTotal . groupByCategory . fromPeriod py) ts

categoryTotal :: [Transaction] -> CategoryTotal
categoryTotal txs = (transactionCategory (head txs), totalTransactions txs)

zipYearlyGroups :: [CategoryTotal] -> [CategoryTotal] -> [YearlyLine]
zipYearlyGroups [] [] = []
zipYearlyGroups ((c,a):gs) [] = YearlyLine c a 0 : zipYearlyGroups gs []
zipYearlyGroups [] ((c,a):gs) = YearlyLine c 0 a : zipYearlyGroups [] gs
zipYearlyGroups ((cc,ca):cgs) ((pc,pa):pgs) | cc < pc = YearlyLine cc ca 0 : zipYearlyGroups cgs ((pc,pa):pgs)
zipYearlyGroups ((cc,ca):cgs) ((pc,pa):pgs) | cc > pc = YearlyLine pc 0 pa : zipYearlyGroups ((cc,ca):cgs) pgs
zipYearlyGroups ((cc,ca):cgs) ((_,pa):pgs) | otherwise = YearlyLine cc ca pa : zipYearlyGroups cgs pgs

yearlyTitle :: YearlySelection -> CategorySelector -> String 
yearlyTitle   ys _ = printf "Yearly report for all categories : %s" (show ys)

yearlyFooter :: YearlySelection -> CategorySelector -> [Transaction] -> String
yearlyFooter (YearlySelection _ cy py) _ ts = printf "%-49s:%10s |%10s" "TOTAL" (show ct) (show pt)
    where
        (ct,pt) = grandTotal $ categoryTotals cy py ts
        
grandTotal :: [YearlyLine] -> (Amount, Amount)
grandTotal yls = ((sum . map yearlyLineCurrentAmount) yls
                 ,(sum . map yearlyLinePreviousAmount) yls)



