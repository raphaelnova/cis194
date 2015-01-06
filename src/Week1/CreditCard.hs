module Week1.CreditCard where

-- Ex. 1
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev
-- Can zippers **shades on** revert this situation?
--
-- A more efficient way of appending would turn toDigitsRev obsolete, although
-- I don't think an extra dependency would be worth the trouble. Of course you
-- can try it for learning purposes! Someday

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- Ex. 2
-- go to the bottom of the stack and work your way up doing what you must
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
-- That same zipper story applies here I guess

-- Ex. 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigitsRev)

-- Ex. 4
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
-- It would've been easier to drop the `reverse` in toDigits and then
-- `doubleEveryOther` counting from the beggining of the list, but solving
-- the problem by its parts isn't without its faults right?

