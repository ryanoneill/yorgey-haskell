{-# OPTIONS_GHC -Wall #-}

-- converts Integer to list of digits
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- converts Integer to list of digits in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = digit : toDigitsRev remaining
      where digit     = mod n 10
            remaining = div n 10

-- applies a function to an item if the associated boolean is True
mapIfTrue :: (a -> a) -> [(a, Bool)] -> [a]
mapIfTrue f xs = map (\x -> if snd x then f (fst x) else fst x) xs

-- double every other number starting from the right
-- [8,7,6,5] -> [16,7,12,5], [1,2,3] -> [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = mapIfTrue (*2) $ zip n $ cycle rep
  where
    rep = [even len, odd len]
    len = length n

-- sum of all digits
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- checks whether Integer is a multiple of 10
isMultipleOf10 :: Integer -> Bool
isMultipleOf10 n = mod n 10 == 0

-- validates a credit card number
validate :: Integer -> Bool
validate = isMultipleOf10 . sumDigits . doubleEveryOther . toDigits
