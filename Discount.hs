module Discount (module Discount) where

import           Data.List              (find)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict        (Map)
import           Data.Ratio

import           Discount.Types

type Discounted = Either Product DiscountedProduct

computeDiscounts
    :: Map Int Product
    -> Map Int Discount
    -> Order Int
    -> Maybe [LineItem Discounted]
computeDiscounts productDb discountDb (Order lineItems discountCode) = case discountCode of
    Nothing -> go Left
    Just code -> case findCode code of
        Nothing       -> computeDiscounts productDb discountDb (Order lineItems Nothing)
        Just discount -> case discountType discount of
            AllProducts    -> go (Right . applyDiscount discount)
            ProductList ls -> sequenceA $ map (discountedItem discount ls) lineItems
    where
        go fn = (fmap . fmap) fn <$> traverse (traverse (`Map.lookup` productDb)) lineItems
        findCode code = find ((code ==) . discountName) (Map.elems discountDb)
        discountedItem discount ls (LineItem i p) =
            flip LineItem p <$> discounter discount ls i
        discounter discount ls item = (if item `elem` ls
            then Right . applyDiscount discount
            else Left) <$> Map.lookup item productDb

applyDiscount :: Discount -> Product -> DiscountedProduct
applyDiscount discount@(Discount _ percent _) product@(Product _ price) =
    DiscountedProduct product discounted
    where discounted = round $ (toInteger (100 - percent) % 100) * fromIntegral price
