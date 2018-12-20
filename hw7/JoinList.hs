{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module JoinList where


import Data.Monoid
import Sized
import Test.QuickCheck
import Scrabble
import Buffer
import Editor


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)


(+++):: Monoid m => JoinList m a-> JoinList m a-> JoinList m a
Empty +++ x = x
x +++ Empty = x
a@(Single a1 c1) +++ b@(Single a2 c2) = Append (a1 `mappend` a2) (Single a1 c1 ) (Single a2 c2)
a@(Single a1 c1) +++ b@(Append p l r) = Append (a1 `mappend` p) a b
a@(Append p l r)  +++ b@(Single a1 c1)  = Append (p `mappend` a1) a b
a@(Append p1 l1 r1)  +++ b@(Append p2 l2 r2)  = Append (p1 `mappend` p2) a b


(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

{-
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ x y= (jlToList y) !!? x
-}
