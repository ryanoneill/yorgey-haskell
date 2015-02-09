module Golf where

import Data.List (group, intercalate, sort, tails, transpose, zip3)
import Data.Maybe (fromMaybe)

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

histogram xs = (intercalate "\n" $ transpose stars) ++ "\n==========\n0123456789\n"
  where
    stars = map (\i -> replicate (maximum counts - i) ' ' ++ replicate i '*') counts
    counts = map (fromMaybe 0 . flip lookup db) [0..9]
    db = map (\l -> (head l, length l)) $ group $ sort xs
