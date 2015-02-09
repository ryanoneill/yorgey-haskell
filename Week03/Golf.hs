module Golf where

import Data.List (tails, zip3)

skips :: [a] -> [[a]]
skips = map skipper . init . tails . zip [1..]

skipper :: Integral a => [(a,b)] -> [b]
skipper xs = map snd $ filter p xs
  where
    p = (==0) . flip mod n . fst
    n = fst $ head xs

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map s $ filter p $ zip3 xs ys zs
  where
    s  = \(x,y,z) -> y
    p  = \(x,y,z) -> y > x && y > z
    zs = tail ys
    ys = tail xs
