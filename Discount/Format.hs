module Discount.Format where

import Discount.Types

showPrice :: Int -> String
showPrice price = let
    (dollars,cents) = divMod price 100
    in "$" ++ show dollars ++ "." ++ show cents

pluraliseCopy :: Int -> String
pluraliseCopy 1 = show 1 ++ " copy"
pluraliseCopy n = show n ++ " copies"

formatDiscountedLineItem :: LineItem Discounted -> String
formatDiscountedLineItem lineItem@(LineItem product quantity) = let
    total = calculateLineItemTotal lineItem
    in show total
