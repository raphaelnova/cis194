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
    describe "Week 2 - Ex. 2" $ do
        -- TODO: Learn how to autogenerate MessageTree instances with QC and
        -- how to write props.
        -- https://github.com/nick8325/quickcheck/blob/master/examples/Set.hs
        describe "insert" $ do
            it "ignores Unknown insertions" $ do
                insert (Unknown "") Leaf `shouldBe` Leaf
            it "inserts in empty trees" $ do
                insert (LogMessage Info 1 "") Leaf
                  `shouldBe` Node Leaf (LogMessage Info 1 "") Leaf
            it "inserts in the correct branch" $ do
                let m1 = (LogMessage Info 1 "")
                    m2 = (LogMessage Info 2 "")
                insert m1 (Node Leaf m2 Leaf)
                  `shouldBe` Node (Node Leaf m1 Leaf) m2 Leaf
                insert m2 (Node Leaf m1 Leaf)
                  `shouldBe` Node Leaf m1 (Node Leaf m2 Leaf)
    describe "Week 2 - Ex. 5" $ do
        describe "whatWentWrong" $ do
            let logs = [ LogMessage Info 1 "Info"
                       , LogMessage Warning 2 "Warn"
                       , LogMessage (Error 10) 0 "Low sev error"
                       , LogMessage (Error 60) 9 "Second high sev error"
                       , LogMessage (Error 50) 5 "First high sev error"
                       ]
            it "filters log entries by type and size" $ do
                length (whatWentWrong logs) `shouldBe` 2
            it "orders log entries by timestamp" $ do
                whatWentWrong logs `shouldBe` [ "First high sev error"
                                              , "Second high sev error" ]

