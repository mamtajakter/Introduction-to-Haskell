{-# LANGUAGE FlexibleContexts #-}


module Fibonacci where

-- Homework 6

--1--
fib :: Integer -> Integer
fib 0=0
fib 1=1
fib x= fib (x-1)+fib (x-2)

fibs1 :: [Integer]
fibs1 =  map fib [0..]

--2--found in stack overflow

fibs2 :: [Integer]
fibs2 = 0 : 1 : 1 : zipWith (+) fibs2 (tail fibs2)

--3-- (streamToList 1) gives error :(
data Stream a= Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y)= x : streamToList y

--ListOfTwentyStream= take 20 (streamToList s)

instance Show a => Show (Stream a) where
          show = show . take 20 . streamToList


--4--

streamRepeat :: a -> Stream a
streamRepeat x= Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))


--5--

nats :: Stream Integer
nats = Cons 0 (streamInfinite 1)

streamInfinite :: Integer -> Stream Integer
streamInfinite x= Cons x (streamInfinite (x+1))

--ruler :: Stream Integer
--ruler

--6--

x :: Stream Integer
x= Cons 0 (streamSequence 1)

streamSequence :: Integer -> Stream Integer
streamSequence y= Cons 1 (streamSequence (y+1))
