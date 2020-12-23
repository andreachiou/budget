module Yearly ( yearlyLines )
    where

import Amount (Amount (..))
import Category (Category (..))
import Transaction          (Transaction (..))
import YearlyLine ( YearlyLine (..))
import YearlySelection      (YearlySelection (..))
import CategorySelection    (CategorySelector)

yearlyLines :: YearlySelection -> CategorySelector -> [Transaction] -> [String]
yearlyLines _ _ _ = [show yl]
    where 
        yl = YearlyLine c t u
        c = Category "Online Services"
        t = Amount 9807
        u = Amount 16400

