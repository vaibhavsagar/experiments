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
computeDiscounts productDb discountDb (Order lineItems discountCode) =
    case discountCode of
        Nothing -> go Left
        Just code -> case find (\d -> discountName d == code) (Map.elems discountDb) of
            Nothing       -> computeDiscounts productDb discountDb (Order lineItems Nothing)
            Just discount -> case discountType discount of
                AllProducts    -> go (Right . applyDiscount discount)
                ProductList ls -> let
                    discounter item = if item `elem` ls
                        then Right . applyDiscount discount <$> Map.lookup item productDb
                        else Left                           <$> Map.lookup item productDb
                    discountedItem (LineItem i p) = flip LineItem p <$> discounter i
                    in sequenceA $ map discountedItem lineItems
    where go fn = (fmap . fmap) fn <$> traverse (traverse (`Map.lookup` productDb)) lineItems

applyDiscount :: Discount -> Product -> DiscountedProduct
applyDiscount discount@(Discount _ percent _) product@(Product _ price) =
    DiscountedProduct product discounted
    where discounted = round $ (toInteger (100 - percent) % 100) * fromIntegral price
