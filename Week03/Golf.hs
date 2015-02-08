module Golf where

import Data.List (tails)

skips :: [a] -> [[a]]
skips = map skipper . init . tails . zip [1..]

skipper :: Integral a => [(a,b)] -> [b]
skipper xs = map snd $ filter p xs
  where
    p = (==0) . flip mod n . fst
    n = fst $ head xs
