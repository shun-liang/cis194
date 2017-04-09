{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessageWords :: [String] -> LogMessage
parseMessageWords ("I" : timeStr : restWords) = LogMessage Info (read timeStr :: Int) (unwords restWords)
parseMessageWords ("W" : timeStr : restWords) = LogMessage Warning (read timeStr :: Int) (unwords restWords)
parseMessageWords ("E" : errorStr : timeStr : restWords) = 
    LogMessage (Error (read errorStr :: Int)) (read timeStr :: Int) (unwords restWords)
parseMessageWords strList = Unknown (unwords strList)

parseMessage :: String -> LogMessage
parseMessage string = parseMessageWords (words string)

parse :: String -> [LogMessage]
parse text = map parseMessage (lines text)
