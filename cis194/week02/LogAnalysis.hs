module LogAnalysis where

import           Log
-- import Data.List

main :: IO()
main = do
    print "Homework 02"
    print "Algebraic Data Types"

-- EXERCISE 1

parseMessage :: String -> LogMessage
parseMessage logLine = parseWords (words logLine)
    where   parseWords ("I":ts:msg) = LogMessage Info
                (read ts :: Int) (unwords msg)
            parseWords ("W":ts:msg) = LogMessage Warning
                (read ts :: Int) (unwords msg)
            parseWords ("E":lvl:ts:msg) = LogMessage (Error (read lvl :: Int))
                (read ts :: Int) (unwords msg)
            parseWords _ = Unknown logLine

parse :: String -> [LogMessage]
parse str = [parseMessage msg | msg <- lines str]

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ tsMsg _) (Node a logTree@(LogMessage _ tsTree _) b)
    | tsMsg < tsTree = Node (insert logMsg a) logTree b
    | otherwise = Node a logTree (insert logMsg b)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                      = []
inOrder (Node treeA logMsg treeB) = inOrder treeA ++ [logMsg] ++ inOrder treeB

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMsgs = [msgToStr x | x@(LogMessage (Error lvl) _ _) <- sortedLogs, lvl > 50]
    where sortedLogs = inOrder (build logMsgs)

msgToStr :: LogMessage -> String
msgToStr (Unknown msg) = msg
msgToStr (LogMessage Info ts msg) = "I" ++ show ts ++ msg
msgToStr (LogMessage (Error lvl) ts msg) = "E " ++ show lvl ++ " " ++ show ts ++ msg
msgToStr (LogMessage Warning ts msg) = "W" ++ show ts ++ msg
