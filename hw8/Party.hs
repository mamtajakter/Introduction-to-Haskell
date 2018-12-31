{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree
import System.IO
import Data.List
--1.1

glCons :: Employee-> GuestList-> GuestList
glCons e@(Emp _ f) (GL em tf)= GL (e:em) (tf+f)

--1.2
instance Monoid GuestList where
  mempty                           = GL [] 0
  (GL [] 0) `mappend` y            = y
  x `mappend` (GL [] 0)            = x
  (GL xs tf1) `mappend` (GL ys tf2)= GL (xs++ys) (tf1+tf2)

instance Semigroup GuestList where
  (GL [] x) <> (GL [] y) = GL [] 0
  (GL [] x) <> (GL xs y) = GL xs y
  (GL xs x) <> (GL [] y) = GL xs x
  (GL xs x) <> (GL ys y) = GL (xs ++ ys) (x+y)

--1.3
moreFun :: GuestList-> GuestList -> GuestList
moreFun x y = case (compare x y) of
         GT   -> x
         LT   -> y
         EQ   -> x

--2

treeFold:: (a->b -> b) ->b-> Tree a -> b
treeFold f acc (Node root forest) = f root (foldr (flip (treeFold f)) acc forest)

--3
nextLevel :: Employee -> [(GuestList, GuestList)]-> (GuestList, GuestList)
nextLevel emp gl= (glCons emp (fst (mconcat gl)), snd (mconcat gl))

--4
maxFun :: Tree Employee-> GuestList
maxFun t= uncurry moreFun (maxFunPair t)

maxFunPair :: Tree Employee -> (GuestList, GuestList)
maxFunPair t@(Node emp [])= (glCons emp mempty, mempty)
maxFunPair t@(Node emp forest) = nextLevel emp (map maxFunPair forest)


--5

totalFun :: GuestList -> Fun
totalFun (GL _ f)=f

guestNames :: GuestList -> [Name]
guestNames (GL emps _)= sort (map empName emps)

totalGuestList :: Tree Employee -> [Name]
totalGuestList t= ("Total Fun : " ++ show (totalFun (maxFun t))) : guestNames (maxFun t)

main :: IO()
main= do
    handle<- openFile "company.txt" ReadMode
    contents<- hGetContents handle
    let gl= totalGuestList (read contents :: Tree Employee)
    mapM_ putStrLn gl
    hClose handle
