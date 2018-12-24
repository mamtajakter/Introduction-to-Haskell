{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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
          show = concatMap (\x-> x ++ ",") . map show . take 50 . streamToList

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
ruler :: Stream Integer
ruler = streamMap divisibilityCheck (streamFromSeed  succ 1)

divisibilityCheck x
          | even x    = 1+divisibilityCheck (x `div` 2)
          | otherwise = 0
--6--

x :: Stream Integer
x= Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
        fromInteger x= Cons x (streamRepeat 0)
        negate x= streamMap negate x
        a@(Cons a1 a2) + b@(Cons b1 b2) = Cons (a1+b1) (a2+b2)
        a@(Cons a1 a2) * b@(Cons b1 b2) = Cons (a1*b1) (streamMap (a1*) b2 + a2 * b)


instance Fractional (Stream Integer) where
        a@(Cons a0 a') / b@(Cons b0 b') = Cons (a0 `div` b0) (streamMap (`div` b0) (a'-(a / b)*b'))


fibs3:: Stream Integer
fibs3 = x / (1-x-x^2)

--7--

data Matrix = Matrix (Integer, Integer) (Integer, Integer)
      deriving Show


instance Num Matrix where
      a@(Matrix a1@(a11,a12) (a21,a22)) * b@(Matrix b1@(b11,b12) b2@(b21,b22)) = Matrix (a11*b11+a12*b21, a11*b12+a12*b22) (a21*b11+a22*b21, a21*b12+a22*b22)


fib4 :: Integer-> Integer
fib4 x
     | x==0 = 0
     | otherwise = getValue (m^x)

m=Matrix (1,1) (1,0)

getValue:: Matrix -> Integer
getValue (Matrix (_,a2) (_,_))= a2
