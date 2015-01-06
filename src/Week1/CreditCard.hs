module Week1.CreditCard where

toDigits :: Int -> [Int]
toDigits = reverse . toDigitsRev

toDigitsRev :: Int -> [Int]
toDigitsRev n
    | n <= 0    = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

