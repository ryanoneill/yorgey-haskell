{-# OPTIONS_GHC -Wall #-}

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3
  | n <= 0    = []
  | n == 1    = [(p1,p2)]
  | otherwise = moves1 ++ moves2 ++ moves3
      where
        moves1 = hanoi (n-1) p1 p3 p2
        moves2 = hanoi 1     p1 p2 p3
        moves3 = hanoi (n-1) p3 p2 p1
