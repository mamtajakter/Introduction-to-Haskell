module Lecture3 where

data IntList = Empty | Cons Int IntList
         deriving Show

addOneToAll :: IntList -> IntList
addOneToAll Empty= Empty
addOneToAll (Cons x xs)= Cons (x+1) (addOneToAll xs)

absAll :: IntList -> IntList
absAll Empty= Empty
absAll (Cons x xs)= Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty= Empty
squareAll (Cons x xs)= Cons (x*x) (squareAll xs)


exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x+1
square x = x*x

mapIntList:: IntList-> (Int-> Int)-> IntList
mapIntList Empty f       = Empty
mapIntList (Cons x xs) f = Cons (f x) (mapIntList xs f)

keepOnlyPositive :: IntList-> IntList
keepOnlyPositive Empty = Empty
keepOnlyPositive (Cons x xs)
                      | x>0       = Cons x (keepOnlyPositive xs)
                      | otherwise = keepOnlyPositive xs

keepOnlyEven :: IntList-> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
                      | even x       = Cons x (keepOnlyEven xs)
                      | otherwise    = keepOnlyEven xs

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList p Empty = Empty
filterIntList p (Cons x xs)
                  | p x       = Cons x (filterIntList p xs)
                  | otherwise = filterIntList p xs


data List t =E | C t (List t)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2= C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)


filterList :: (t -> Bool) -> List t-> List t
filterList p E = E
filterList p (C x xs)
                  | p x       = C x (filterList p xs)
                  | otherwise = filterList p xs


mapList:: List a-> (a-> b)-> List b
mapList E f       = E
mapList (C x xs) f = C (f x) (mapList xs f)


data Maybe a = Nothing | Just a


doStuff1 :: [Int]-> Int
doStuff1 []=0
doStuff1 [_]=0
doStuff1 xs= head xs + head (tail xs)


doStuff2 :: [Int]-> Int
doStuff2 []=0
doStuff2 [_]=0
doStuff2 (x:y:_)= x+y

safeHead:: [a]-> Maybe a
safeHead []=Nothing
safeHead (x:_)= Just x

data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a]-> Maybe (NonEmptyList a)
listToNel []=Nothing
listToNel (x:xs)= Just $ NEL x xs

headNel :: NonEmptyList a -> a
headNel (NEL x _)= x

tailNel ::  NonEmptyList a -> [a]
tailNel (NEL _ xs)=xs
