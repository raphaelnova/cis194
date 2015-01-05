module Week1 (specs) where

import Test.Hspec
import Test.QuickCheck

import Week1.CreditCard

specs :: SpecWith ()
specs = do
    describe "Week 1 - Credit card" $ do
        describe "toDigits" $ do
            it "always returns a singleton list" $ property $ \n ->
                toDigits n == [n]

