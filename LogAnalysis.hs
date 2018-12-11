{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Prelude

parseMessage :: String -> LogMessage
parseMessage s = let  msg=(words s)
                      int1=read (msg !! 1) :: Int
                      int2= read (msg !! 2):: Int in
                 case msg !! 0 of
                   "E" -> LogMessage (Error int1 )  int2 (unwords (drop 3 msg))
                   "I" -> LogMessage Info int1 (unwords (drop 3 msg))
                   "W" -> LogMessage Warning int1 (unwords (drop 3 msg))
                   _   -> Unknown s

parse :: String -> [LogMessage]
parse x= parse' (lines x)
      where
           parse'   []    = []
           parse' (y:ys) = [parseMessage y] ++ parse' ys



insert :: LogMessage -> MessageTree-> MessageTree
insert (Unknown _) Leaf= Leaf
insert (Unknown _) (Node l m r) = Node l m r
insert lm@(LogMessage _ _ _) Leaf =  Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node l mm@(LogMessage _ mts _) r)
                  | ts < mts      =Node (insert lm l) mm r
                  | ts > mts      =Node l mm (insert lm r)
                  | otherwise     = Node l lm r

build :: [LogMessage]-> MessageTree
build []=Leaf
build (x:[])= Node Leaf x Leaf
build (x:xs) =  createTree (Node Leaf x Leaf) xs
              where
                  createTree tr []=tr
                  createTree tr (t:ts) = createTree (insert t tr) ts

inOrder:: MessageTree -> [LogMessage]
inOrder Leaf= []
inOrder (Node l m r)= inOrder l ++ [m] ++ inOrder r

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
