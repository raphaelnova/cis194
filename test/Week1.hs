module Week1 (specs) where

import Test.Hspec
import Test.QuickCheck

import Week1.CreditCard

specs :: SpecWith ()
specs = do
    describe "Week 1 - Ex. 1" $ do
        describe "toDigits" $ do
            it "returns an empty list for negative numbers" $ do
                toDigits (-42) `shouldBe` []
            it "returns an empty list for zero" $ do
                toDigits 0 `shouldBe` []
            it "returns the correct digits" $ do
                toDigits 12345 `shouldBe` [1,2,3,4,5]
                toDigits 100   `shouldBe` [1,0,0]
        describe "toDigitsRev" $ do
            it "returns an empty list for negative numbers" $ do
                toDigitsRev (-42) `shouldBe` []
            it "returns an empty list for zero" $ do
                toDigitsRev 0 `shouldBe` []
            it "is the reverse list of toDigits" $ property $ \n ->
                toDigitsRev n == reverse (toDigits n)
    describe "Week 1 - Ex. 2" $ do
        describe "doubleEveryOther" $ do
            it "outputs a list the same size of the input" $ property $ \ls ->
                length (doubleEveryOther ls) == length ls
            it "doesn't change a singleton list" $ property $ \n ->
                doubleEveryOther [n] == [n]
            it "counts from the end of the list" $ property $ \e o ->
                doubleEveryOther       [e, o] ==         [2*e, o] &&
                doubleEveryOther    [o, e, o] ==      [o, 2*e, o] &&
                doubleEveryOther [e, o, e, o] == [2*e, o, 2*e, o]

