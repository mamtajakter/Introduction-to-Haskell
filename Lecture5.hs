module Lecture5 where

{-
--Not work

f :: a-> a-> a

f a1 a2 = a1 && a2

f a1 a2 = a1+a2

f a1 a2 = a1++ "" ++ a2

f a1 a2 = case (typeOf a1) of
           Int-> a1 + a2
           Bool -> a1 && a2
           _    -> a1
-}


f :: a-> b
f x=  f x


fun :: (a->a)-> a -> a
fun f x = f x


data Foo= F Int | G Char

instance Eq Foo where
    (F i1) == (F i2) = i1 == i2
    (G c1) == (G c2) = c1 == c2
    _      == _      = False

    foo1 /= foo2 = not (foo1 == foo2)


class Listable a where
     toList :: a-> [Int]


instance Listable Int where
     toList x= [x]

instance Listable Bool where
     toList True= [1]
     toList False= [0]


data Tree a = Empty | Node a (Tree a) (Tree a)


instance Listable (Tree Int) where
      toList Empty= []
      toList (Node x l r)= toList l ++ [x] ++ toList r

---lets use toList

instance (Listable a, Listable b) => Listable (a,b) where
      toList (x,y)= toList x ++ toList y

sumL x = sum (toList x)


foo x y = sum (toList x) == sum(toList y) || x<y
