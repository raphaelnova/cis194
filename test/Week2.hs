module Week2 (specs) where

import Test.Hspec
import Test.QuickCheck

import Week2.LogAnalysis
import Week2.Log

specs :: SpecWith ()
specs = ex1

ex1 :: SpecWith ()
ex1 = do
    describe "Week 2 - Ex. 1" $ do
        describe "parseMessage" $ do
            it "parses info log messages" $ property $ \time msg ->
                case parseMessage ("I " ++ show (time :: Int) ++ " " ++ msg) of
                    Unknown _ -> False
                    _         -> True
            it "parses warn log messages" $ property $ \time msg ->
                case parseMessage ("W " ++ show (time :: Int) ++ " " ++ msg) of
                    Unknown _ -> False
                    _         -> True
            it "parses error log messages" $ property $ \sev time msg ->
                let str = ("E " ++ show (sev  :: Int)
                         ++ " " ++ show (time :: Int) ++ " " ++ msg)
                in case parseMessage str of
                    Unknown _ -> False
                    _         -> True

