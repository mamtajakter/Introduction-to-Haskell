{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module JoinList where

--import Test.QuickCheck
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

testList1 :: JoinList Size String
testList1 = (Append (Size 4)
             (Append (Size 2)
              (Single (Size 1) "moon")
              (Single (Size 1) "planet"))
             (Append (Size 2)
              (Single (Size 1) "star")
              (Single (Size 1) "galaxy")))
--1--

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)


(+++):: Monoid m => JoinList m a-> JoinList m a-> JoinList m a
Empty +++ x = x
x +++ Empty = x
a +++ b = Append ((tag a) `mappend` (tag b)) a b


tag :: Monoid m=> JoinList m a-> m
tag (Single x _)= x
tag (Append x _ _)= x


--2.1


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ x _ | x<0            = Nothing
indexJ x Empty              = Nothing
indexJ x jl | x>=tagSize jl = Nothing
indexJ x (Single y z)
       | x==0               = Just z
       | otherwise          = Nothing
indexJ x jl@(Append y l r)
       | x < tagSize l = indexJ x l
       | otherwise     = indexJ (x- tagSize l) r


tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


testIndexJ:: Int -> Bool
testIndexJ i= (indexJ i testList1) == (jlToList testList1 !!? i)

--2.2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n<0            = error "Number cant be negative"
dropJ n jl | n==0           = jl
dropJ _ Empty               = Empty
dropJ n jl | n==tagSize jl  = Empty
dropJ n jl | n>tagSize jl   = error "Insufficient elements in JoinList"
dropJ n jl@(Single y z)     = Empty
dropJ n jl@(Append x l r)
       | n <= tagSize l      = dropJ n l +++ r
       | otherwise          = dropJ (n- tagSize l) r


testDropJ :: Int-> Bool
testDropJ n= jlToList (dropJ n testList1) == drop n (jlToList testList1)

--2.3
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl | n<0            = error "Number cant be negative"
takeJ n jl | n==0            = Empty
takeJ _ Empty                = Empty
takeJ n jl | n==tagSize jl   = jl
takeJ n jl | n>tagSize jl = error "Insufficient elements in JoinList"
takeJ n jl@(Single y z)= jl
takeJ n jl@(Append x l r)
       | n <= tagSize l =  takeJ n l
       | otherwise     = l +++ takeJ (n- tagSize l) r


testTakeJ :: Int-> Bool
testTakeJ n= jlToList (takeJ n testList1) == take n (jlToList testList1)


--3
scoreLine :: String-> JoinList Score String
scoreLine "" = Empty
scoreLine x  = Single (scoreString x) x

--4

scoreSizeLine :: String-> JoinList (Score, Size) String
scoreSizeLine "" = Empty
scoreSizeLine x  = Single (scoreString x, Size 1) x

tagScore ::  JoinList (Score, Size) String -> Int
tagScore jl     = getScore (fst (tag jl))

instance Buffer (JoinList (Score, Size) String) where
      toString jl = unlines (jlToList jl)

      fromString xs = fromLines (lines xs)
             where fromLines [] = Empty
                   fromLines (x:[]) = scoreSizeLine x
                   fromLines ful@(x:xs)= fromLines (take half ful) +++ fromLines (drop half ful)
                                     where half = (length ful) `div` 2

      line= indexJ

      replaceLine x _ jl | x<0            = jl
      replaceLine x _ Empty               = Empty
      replaceLine x _ jl | x>=tagSize jl  = jl
      replaceLine x s jl= takeJ x jl +++ scoreSizeLine s +++ dropJ (x+1) jl

      numLines =tagSize
      value = tagScore

main = runEditor editor $ newBuf

newBuf :: JoinList (Score, Size) String
newBuf = fromString $ unlines
         [ "This buffer is for poems you want to discard,"
         , "and for tinkering with steam valve coefficients."
         , "To load a different file, type the character 'l'"
         , "followed by the name of the file."
         ]
