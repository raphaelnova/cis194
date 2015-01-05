module Main (main) where

import Test.Hspec

import qualified Week1

main :: IO ()
main = hspec $ do
    Week1.specs

