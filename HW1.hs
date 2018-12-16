module Homework1 where

import Data.List



--1--
toDigits :: Integer -> [Integer]
toDigits x
         | x<=0      = []
         | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]


toDigitsRev :: Integer -> [Integer]
toDigitsRev x
         | x<=0      = []
         | otherwise = [x `mod` 10] ++ toDigitsRev (x `div` 10)

--2--
doubleEveryOther:: [Integer]-> [Integer]
doubleEveryOther xxs = reverse (h1 (reverse xxs))
           where h1 :: [Integer]-> [Integer]
                 h1 []=[]
                 h1 (x:[])=[x]
                 h1 (x:y:xs)= x : (2*y) : h1 xs





--3--


sumDigits :: [Integer]-> Integer ---List to sum
sumDigits []=0
sumDigits (x:xs)= sum (toDigits x) + sumDigits xs


--4--

validate:: Integer-> Bool
validate n
         | sumDigits (doubleEveryOther (toDigits n)) `mod` 10 ==0 = True
         | otherwise      = False



--5--
type Peg= String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 source dest aux= [(source, dest)]
hanoi disk source dest aux= hanoi (disk - 1) source aux dest ++ [(source,dest)] ++ hanoi (disk - 1) aux dest source


--6--
hanoi4 :: Integer -> Peg -> Peg-> Peg-> Peg-> [Move]
hanoi4 0 source dest aux1 aux2 =  []
hanoi4 1 source dest aux1 aux2 =  [(source, dest)]
hanoi4 disk source dest aux1 aux2 = hanoi4 (disk - 2) source aux1 aux2 dest ++ [(source,aux2),(source,dest), (aux2,dest)] ++ hanoi4 (disk - 2) aux1 dest source aux2
