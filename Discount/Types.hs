{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Discount.Types (module Discount.Types) where

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
