{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Prelude

-- Homework 2

--1--


parseMessage :: String -> LogMessage
parseMessage s = let  (x:y:z:msg)=(words s) in
                 case x of
                   "E" -> LogMessage (Error (read y::Int) )  (read z::Int) (unwords msg)
                   "I" -> LogMessage Info (read y::Int) (unwords msg)
                   "W" -> LogMessage Warning (read y::Int) (unwords msg)
                   _   -> Unknown s

parse :: String -> [LogMessage]
parse x= parse' (lines x)
      where
           parse'   []    = []
           parse' (y:ys) = [parseMessage y] ++ parse' ys

--2--
insert :: LogMessage -> MessageTree-> MessageTree
insert (Unknown _) t= t
insert lm@(LogMessage _ _ _) Leaf =  Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node l mm@(LogMessage _ mts _) r)
                  | ts < mts      =Node (insert lm l) mm r
                  | ts > mts      =Node l mm (insert lm r)
                  | otherwise     = Node l lm r


--3--

build :: [LogMessage]-> MessageTree
build []=Leaf
build (x:[])= Node Leaf x Leaf
build (x:xs) =  createTree (Node Leaf x Leaf) xs
              where
                  createTree tr []=tr
                  createTree tr (t:ts) = createTree (insert t tr) ts


--4--
inOrder:: MessageTree -> [LogMessage]
inOrder Leaf= []
inOrder (Node l m r)= inOrder l ++ [m] ++ inOrder r


--5--

whatWentWrong' :: [LogMessage] -> [String]
whatWentWrong' [] = []
whatWentWrong' ((LogMessage mtype _ msg):xs)= case mtype of
                                                    Error severity -> if severity>50
                                                                      then [msg] ++ whatWentWrong' xs
                                                                        else whatWentWrong' xs
                                                    _       -> whatWentWrong' xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong []=[]
whatWentWrong xs=  whatWentWrong' (inOrder (build xs))


--testParse parse 10 "error.log"
--testWhatWentWrong parse whatWentWrong "sample.log"
