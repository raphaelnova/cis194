module Week1.Hanoi where

-- Ex. 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi disks origin dest aux =
    hanoi (disks - 1) origin aux dest
    ++ [(origin, dest)]
    ++ hanoi (disks - 1) aux dest origin

-- Ex. 6
-- 15 disks in 305 moves... not bad, but not optimal
-- TODO: Come back here with the optimal solution
fourPegHanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
fourPegHanoi 0 _ _ _ _ = []
fourPegHanoi disks origin dest auxOne auxTwo =
    fourPegHanoi topHalf origin auxOne dest auxTwo
    ++ hanoi bottomHalf origin auxTwo dest
    ++ [(origin, dest)]
    ++ hanoi bottomHalf auxTwo dest origin
    ++ fourPegHanoi topHalf auxOne dest auxTwo origin
  where
    disksToMove = fromIntegral $ disks - 1
    topHalf     = ceiling ( disksToMove / 2 :: Double )
    bottomHalf  = floor   ( disksToMove / 2 :: Double )

