{-# LANGUAGE FlexibleContexts #-}


module Fibonacci where


import Data.List
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
fibs2 = 0 : 1 :  zipWith (+) fibs2 (tail fibs2)

--3-- (streamToList 1) gives error :(
data Stream a= Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y)= x : streamToList y

--ListOfTwentyStream= take 20 (streamToList s)



instance Show a => Show (Stream a) where
          show = foldr (\x acc-> show x ++ ", " ++ acc ) "" . map show . take 20 . streamToList


--4--

streamRepeat :: a -> Stream a
streamRepeat x= Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))


--5--

nats :: Stream Integer
nats = streamFromSeed succ 0


-- First version with divisibility tests
ruler0 :: Stream Integer
ruler0 = streamMap (\n -> rulerValue n (floor (log2 n))) $
         streamFromSeed succ 1

log2 :: Integer -> Float
log2 n = log (fromInteger n) / log 2

divides :: Integer -> Integer -> Bool
divides n m = m `mod` n == 0

rulerValue :: Integer -> Integer -> Integer
rulerValue n power = if (2^power) `divides` n
                     then power
                     else rulerValue n (pred power)

--6--

x :: Stream Integer
x= Cons 0 (streamSequence 1)

streamSequence :: Integer -> Stream Integer
streamSequence y= Cons 1 (streamSequence (y+1))
