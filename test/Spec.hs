module Main (main) where

import Test.Hspec

import qualified Week1
import qualified Week2

main :: IO ()
main = hspec $ do
    Week1.specs
    Week2.specs

