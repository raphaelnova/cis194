module Week1.CreditCard where

-- Ex. 1
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- Ex. 2
-- go down the stack and work your way up doing what you must
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = snd . countFromBottom
  where
    countFromBottom :: [Integer] -> (Int, [Integer])
    countFromBottom []     = (0, [])
    countFromBottom (x:xs) =
        let (prevIdx, ys)  = countFromBottom xs
            currIdx        = prevIdx + 1
        in if even currIdx
            then (currIdx, 2*x : ys)
            else (currIdx,   x : ys)

