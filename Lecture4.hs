module Lecture4 where

import Prelude


gt100 :: Integer -> Bool
gt100 x= x>100

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs

greaterThan100_2 :: [Integer]-> [Integer]
greaterThan100_2 xs = filter (\x-> x>100) xs


greaterThan100_3:: [Integer]-> [Integer]
greaterThan100_3 xs= filter (>100) xs


foo :: (b-> c)-> (a-> b)-> (a->c)
foo f g =(\x-> f (g x))


--foo :: (b-> c)-> (a-> b)-> (a->c)
--foo f g =f . g  -- function composition
myTest :: [Integer]-> Bool
myTest xs= even (length (greaterThan100 xs))


myTest' :: [Integer]-> Bool
myTest' = even . length . greaterThan100


f:: Int-> Int-> Int
f x y= 2*x +y


foobar :: [Integer]-> Integer
foobar []=0
foobar (x:xs)
        | x>3 = (7*x+2) + foobar xs
        | otherwise = foobar xs

foobar' :: [Integer]-> Integer
foobar' = sum . map (\x-> 7*x+2) . filter (>3)

sum' :: [Integer] -> Integer
sum' []=0
sum' (x:xs)= x+ sum' xs

product' :: [Integer] -> Integer
product' []=1
product' (x:xs)= x* product' xs

length' :: [Integer] -> Integer
length' []=0
length' (_:xs)= 1+ length' xs


fold :: b-> (a-> b->b ) -> [a]-> b
fold z f []= z
fold z f (x:xs)= f x (fold z f xs)

sum''= fold 0 (+)
product'' = fold 1 (*)
length'' = fold 0 (\_-> (1+))
