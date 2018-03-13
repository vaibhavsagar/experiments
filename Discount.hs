module Discount
    ( module Discount
    , module Discount.Types
    ) where

import           Data.Bool              (bool)
import           Data.List              (find)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict        (Map)
import           Data.Ratio             ((%))

import           Discount.Types

computeDiscounts
    :: Map Int Product
    -> Map Int Discount
    -> Order Int
    -> Either String [LineItem Discounted]
computeDiscounts productDb discountDb (Order ls discount) = case discount of
    Nothing -> traverse (traverseLookup noDiscount productDb) ls
    Just code -> case findCode code of
        Nothing ->
            computeDiscounts productDb discountDb (Order ls Nothing)
        Just discount -> case discountType discount of
            AllProducts ->
                traverse (traverseLookup (yesDiscount discount) productDb) ls
            ProductList ps -> traverse (discountedItem discount ps) ls
    where
        lookup = lookupError (\key -> "item " ++ show key ++ " not found")
        traverseLookup f db = traverse $ fmap f . lookup db
        findCode code = find ((code ==) . discountName) (Map.elems discountDb)
        discountedItem discount ls (LineItem i p) =
            flip LineItem p <$> discounter discount ls i
        discounter discount ds item =
            bool noDiscount (yesDiscount discount) (item `elem` ds)
            <$> lookup productDb item

lookupError :: Ord k => (k -> l) -> Map k v -> k -> Either l v
lookupError err table key = case Map.lookup key table of
    Just value -> Right value
    Nothing -> Left (err key)

noDiscount :: Product -> Discounted
noDiscount = Left

yesDiscount :: Discount -> Product -> Discounted
yesDiscount discount = Right . applyDiscount discount

applyDiscount :: Discount -> Product -> DiscountedProduct
applyDiscount discount@(Discount _ percent _) product@(Product _ price) =
    DiscountedProduct product discounted
    where discounted =
            round $ (toInteger (100 - percent) % 100) * fromIntegral price

calculateLineItemTotal :: LineItem Discounted -> Int
calculateLineItemTotal (LineItem product quantity) = case product of
    Left  (Product           _ price) -> price * quantity
    Right (DiscountedProduct _ price) -> price * quantity

calculateTotal :: [LineItem Discounted] -> Int
calculateTotal = sum . map calculateLineItemTotal
