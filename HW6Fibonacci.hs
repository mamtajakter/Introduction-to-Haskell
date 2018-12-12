module Fibonacci where

-- Homework 6


fib :: Integer -> Integer
fib 0=0
fib 1=1
fib x= fib (x-1)+fib (x-2)

fibs1 :: [Integer]
fibs1 = [x..]
       where x= fib x

fibs2 :: [Integer]
fibs2 = 0 : 1 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a= Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y)= x : streamToList y

instance Show a => Show (Stream a) where
          show s@(Cons x y)= show z
                          where z= take 20 (streamToList s)
streamRepeat :: a -> Stream a
streamRepeat x= Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = Cons 0 (streamInfinite 1)

streamInfinite :: Integer -> Stream Integer
streamInfinite x= Cons x (streamInfinite (x+1))

--ruler :: Stream Integer
--ruler

streamInfinite' :: Integer -> Stream Integer
streamInfinite' x= Cons x (streamInfinite (x+1))
