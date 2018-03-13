{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Discount.Types (module Discount.Types) where

import Data.Ratio ((%))

data Product
    = Product
    { productName  :: String
    , productPrice :: Int
    } deriving (Eq, Show)

data LineItem a
    = LineItem
    { lineItem         :: a
    , lineItemQuantity :: Int
    } deriving (Eq, Show, Functor, Foldable, Traversable)

data Order a
    = Order
    { orderLineItems    :: [LineItem a]
    , orderDiscountCode :: Maybe String
    } deriving (Eq, Show)

data Discount
    = Discount
    { discountName       :: String
    , discountPercentage :: Int
    , discountType       :: DiscountType
    } deriving (Eq, Show)

data DiscountType
    = AllProducts
    | ProductList [Int]
    deriving (Eq, Show)

data DiscountedProduct
    = DiscountedProduct
    { originalProduct :: Product
    , newPrice        :: Int
    } deriving (Eq, Show)

type Discounted = Either Product DiscountedProduct

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
