module Discount.Format (formatDiscountedCart) where

import Data.Functor ((<$))
import Data.Monoid (mconcat)
import Text.Printf (printf)

import Discount.Types

showPrice :: Int -> String
showPrice price = let
    (dollars,cents) = divMod price 100
    in "$" ++ show dollars ++ "." ++ printf "%02d" cents

pluraliseCopy :: Int -> String
pluraliseCopy 1 = show 1 ++ " copy"
pluraliseCopy n = show n ++ " copies"

formatDiscountedLineItem :: LineItem Discounted -> String
formatDiscountedLineItem lineItem@(LineItem product quantity) = let
    total = showPrice $ calculateLineItemTotal lineItem
    original = case product of
        Left _ -> ""
        Right (DiscountedProduct orig _) -> let
            originalPrice = calculateLineItemTotal (Left orig <$ lineItem)
            in " (Original Price " ++ showPrice originalPrice ++ ")"
    name = case product of
        Left  p -> productName p
        Right p -> productName $ originalProduct p
    qName = "\"" ++ name ++ "\""
    in mconcat
        [total, original, " ", "for ", pluraliseCopy quantity, " of ", qName]

formatDiscountedCart :: [LineItem Discounted] -> String
formatDiscountedCart lineItems = let
    formattedLineItems = map formatDiscountedLineItem lineItems
    orderTotal = "Total " ++ showPrice (calculateTotal lineItems)
    in unlines $ ["Your cart:", ""] ++ formattedLineItems ++ ["---", orderTotal]
