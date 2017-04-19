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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newMessage@(LogMessage _ newTimestamp _) (Node leftBranch message@(LogMessage _ timestamp _) rightBranch)
  | newTimestamp < timestamp  = Node (insert newMessage leftBranch) message rightBranch
  | otherwise = Node leftBranch message (insert message rightBranch)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (message : []) = Node Leaf message Leaf
build (message : messages) = insert message (build messages)
