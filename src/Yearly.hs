module Yearly ( yearlyLines )
    where

import Amount
import Category
import Transaction          (Transaction (..))
import TransactionList      (fromPeriod, groupByCategory, totalTransactions)
import YearlyLine ( YearlyLine (..))
import YearlySelection      (YearlySelection (..))
import CategorySelection    (CategorySelector)

yearlyLines :: YearlySelection -> CategorySelector -> [Transaction] -> [String]
yearlyLines (YearlySelection cy py) _ ts = map show yls
    where 
        cgs = (map categoryAndTotal . groupByCategory . fromPeriod cy) ts
        pgs = (map categoryAndTotal . groupByCategory . fromPeriod py) ts
        categoryAndTotal txs = (transactionCategory (head txs), totalTransactions txs)
        yls = zipYearlyGroups cgs pgs

zipYearlyGroups :: [(Category,Amount)] -> [(Category,Amount)] -> [YearlyLine]
zipYearlyGroups [] [] = []
zipYearlyGroups ((c,a):gs) [] = YearlyLine c a 0 : zipYearlyGroups gs []
zipYearlyGroups [] ((c,a):gs) = YearlyLine c 0 a : zipYearlyGroups [] gs
zipYearlyGroups ((cc,ca):cgs) ((pc,pa):pgs) | cc < pc = YearlyLine cc ca 0 : zipYearlyGroups cgs ((pc,pa):pgs)
zipYearlyGroups ((cc,ca):cgs) ((pc,pa):pgs) | cc > pc = YearlyLine pc 0 pa : zipYearlyGroups ((cc,ca):cgs) pgs
zipYearlyGroups ((cc,ca):cgs) ((_,pa):pgs) | otherwise = YearlyLine cc ca pa : zipYearlyGroups cgs pgs


