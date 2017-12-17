{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Data.Char
import Data.List
import Data.List.Split
import Log

parseInfo :: [String] -> LogMessage
parseInfo xs = if all isDigit (head xs) then LogMessage Info (read (head xs)::Int) (intercalate " " (tail xs)) else Unknown (intercalate " " (tail xs))

parseWarning :: [String] -> LogMessage
parseWarning xs = if all isDigit (head xs) then LogMessage Warning (read (head xs)::Int) (intercalate " " (tail xs)) else Unknown (intercalate " " (tail xs))

parseError :: [String] -> LogMessage
parseError xs = if all isDigit (head xs) && all isDigit (head (tail xs)) then LogMessage (Error (read (head xs)::Int)) (read (head (tail xs))::Int) (intercalate " " (tail (tail xs))) else Unknown (intercalate " " (tail (tail xs)))

parseMessage :: String -> LogMessage
parseMessage [] = Unknown []
parseMessage str
    | length (words str) < 3 = Unknown str
    | head (words str) == "E" = parseError (tail (words str))
    | head (words str) == "I" = parseInfo (tail (words str))
    | head (words str) == "W" = parseWarning (tail (words str))
    | otherwise = Unknown str

parseList :: [String] -> [LogMessage]
parseList [] = []
parseList (x : xs) = parseMessage x : parseList xs

parse :: String -> [LogMessage]
parse str = parseList (Data.List.Split.splitOn "\n" str)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node l lm1@(LogMessage _ c _) r) = if ts <= c then (Node (LogAnalysis.insert lm l) lm1 r) else (Node l lm1 (LogAnalysis.insert lm r))

buildHelper :: [LogMessage] -> MessageTree -> MessageTree
buildHelper [] mt = mt
buildHelper ls mt = LogAnalysis.buildHelper (tail ls) (LogAnalysis.insert (head ls) mt)

build :: [LogMessage] -> MessageTree
build ls = buildHelper ls Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = inOrder l ++ [lm] ++ inOrder r

whatWentWrongHelp :: [LogMessage] -> [LogMessage]
whatWentWrongHelp [] = []
whatWentWrongHelp (lm@(LogMessage (Error sev) _ _) : xs) = if sev >= 50 then lm : whatWentWrongHelp xs else whatWentWrongHelp xs
whatWentWrongHelp (_ : xs) = whatWentWrongHelp xs

printInfo :: [LogMessage] -> [String]
printInfo [] = []
printInfo ((LogMessage _ _ info) : xs) = info : printInfo xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = printInfo (inOrder (build (whatWentWrongHelp lm)))
