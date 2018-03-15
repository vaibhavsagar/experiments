{-# LANGUAGE QuasiQuotes #-}

import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Discount
import           Test.Hspec

main :: IO ()
main = do
    let productDatabase = Map.fromList
            [ (1, Product "Black Jacobins" 2000)
            , (2, Product "Freedom Is a Constant Struggle" 1500)
            ]
    let discountDatabase = Map.fromList
            [ (1, Discount "WELCOME" 50  AllProducts)
            , (2, Discount "JAC75"   75 (ProductList [1]))
            ]
    let basicDiscount = computeDiscounts productDatabase discountDatabase
    let cart = Order [LineItem 1 2, LineItem 2 3] Nothing
    hspec $ do
        describe "testing discount computation" $ do
            it "doesn't apply any discount when no code is provided" $ do
                let expected = Right
                        [ LineItem (Left (Product "Black Jacobins" 2000)) 2
                        , LineItem (Left (Product "Freedom Is a Constant Struggle" 1500)) 3
                        ]
                let discounted = basicDiscount cart
                discounted `shouldBe` expected
                calculateTotal <$> discounted `shouldBe` Right 8500
            it "applies a 50% off discount to all items" $ do
                let expected = Right
                        [ LineItem (Right (DiscountedProduct (Product "Black Jacobins" 2000) 1000)) 2
                        , LineItem (Right (DiscountedProduct (Product "Freedom Is a Constant Struggle" 1500) 750)) 3
                        ]
                let discounted = basicDiscount cart { orderDiscountCode = Just "WELCOME" }
                discounted `shouldBe` expected
                calculateTotal <$> discounted `shouldBe` Right 4250
            it "applies a 75% discount to only one item" $ do
                let expected = Right
                        [ LineItem (Right (DiscountedProduct (Product "Black Jacobins" 2000) 500)) 2
                        , LineItem (Left (Product "Freedom Is a Constant Struggle" 1500)) 3
                        ]
                let discounted = basicDiscount cart { orderDiscountCode = Just "JAC75" }
                discounted `shouldBe` expected
                calculateTotal <$> discounted `shouldBe` Right 5500
            it "doesn't apply any discount when an invalid code is provided" $ do
                let expected = Right
                        [ LineItem (Left (Product "Black Jacobins" 2000)) 2
                        , LineItem (Left (Product "Freedom Is a Constant Struggle" 1500)) 3
                        ]
                let discounted = basicDiscount cart { orderDiscountCode = Just "FOO" }
                discounted `shouldBe` expected
                calculateTotal <$> discounted `shouldBe` Right 8500
            it "provides a useful error message when a LineItem is not found" $ do
                let discounted  = basicDiscount $ Order [LineItem 3 1] Nothing
                discounted `shouldBe` Left "item 3 not found"
                let discounted' = basicDiscount $ Order [LineItem 3 1] (Just "WELCOME")
                discounted `shouldBe` Left "item 3 not found"
        describe "cart display" $ do
            it "doesn't apply any discount when an invalid code is provided" $ do
                let Right discounted = basicDiscount cart
                let expected = unindent [i|
                    Your cart:

                    $40.00 for 2 copies of "Black Jacobins"
                    $45.00 for 3 copies of "Freedom Is a Constant Struggle"
                    ---
                    Total $85.00
                    |]
                formatDiscountedCart discounted `shouldBe` expected
            it "applies a 50% off discount to all items" $ do
                let Right discounted = basicDiscount cart { orderDiscountCode = Just "WELCOME" }
                let expected = unindent [i|
                    Your cart:

                    $20.00 (Original Price $40.00) for 2 copies of "Black Jacobins"
                    $22.50 (Original Price $45.00) for 3 copies of "Freedom Is a Constant Struggle"
                    ---
                    Total $42.50
                    |]
                formatDiscountedCart discounted `shouldBe` expected
            it "applies a 75% discount to only one item" $ do
                let Right discounted = basicDiscount cart { orderDiscountCode = Just "JAC75" }
                let expected = unindent [i|
                    Your cart:

                    $10.00 (Original Price $40.00) for 2 copies of "Black Jacobins"
                    $45.00 for 3 copies of "Freedom Is a Constant Struggle"
                    ---
                    Total $55.00
                    |]
                formatDiscountedCart discounted `shouldBe` expected
