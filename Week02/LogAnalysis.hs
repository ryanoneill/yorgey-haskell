{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage m = case m of
  'E':_ -> parseError m
  'I':_ -> parseInfo m
  'W':_ -> parseWarning m
  _     -> unknownMessage

unknownMessage :: LogMessage
unknownMessage = Unknown "This is not in the right format"

parseError :: String -> LogMessage
parseError = parseWithId (\i -> Error i)

parseInfo :: String -> LogMessage
parseInfo = parseStandard Info

parseWarning :: String -> LogMessage
parseWarning = parseStandard Warning

parseWithId :: (Int -> MessageType) -> String -> LogMessage
parseWithId f m = parseSegments (f typeId) $ dropAllBut 2 segments
  where
    typeId = readInt $ head $ tail segments
    segments = splitInto 4 m

parseStandard :: MessageType -> String -> LogMessage
parseStandard messageType = parseSegments messageType . dropAllBut 2 . splitInto 3

parseSegments :: MessageType -> [String] -> LogMessage
parseSegments messageType segments =
  LogMessage messageType timestamp message
    where
      timestamp = readInt $ segments !! 0
      message = segments !! 1

readInt :: String -> Int
readInt n = read n :: Int

dropAllBut :: Int -> [a] -> [a]
dropAllBut n xs = drop ((length xs) - n) xs

splitInto :: Int -> String -> [String]
splitInto n xs = segment (/= ' ') xs n

segment :: (a -> Bool) -> [a] -> Int -> [[a]]
segment _ [] _ = [[]]
segment p xs n
  | n <= 1    = [xs]
  | otherwise = partOne : segment p partTwo (n-1)
      where
        partTwo = if (length partOne > 0)
                    then tail $ snd parts
                    else snd parts
        partOne = fst parts
        parts   = span p xs
