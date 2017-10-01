{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char (isDigit)

parse :: String -> [LogMessage]
parse str = map parseMessage $ lines str

parseMessage :: String -> LogMessage
parseMessage msg = getMessage $ words msg

getMessage :: [String] -> LogMessage
getMessage (x:y:z:xs)
    | x == "I" && check y = LogMessage Info (read y :: Int) (unwords (z:xs))
    | x == "W" && check y = LogMessage Warning (read y :: Int) (unwords (z:xs))
    | x == "E" && check y && check z = LogMessage (Error (read y :: Int)) (read z :: Int) (unwords xs)
    | otherwise = Unknown $ unwords (x:y:z:xs)
getMessage xs = Unknown $ unwords xs

check :: [Char] -> Bool
check [] = True
check (x:xs) = isDigit x && check xs
