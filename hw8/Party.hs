{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee

glCons :: Employee-> GuestList-> GuestList
glCons e@(Emp n f) (GL em tf)= GL (em++[e]) (tf+f)

instance Monoid GuestList where
  mempty                           = GL [] 0
  x@(GL [] 0) `mappend` y@(GL ys tf) = y
  x@(GL xs tf) `mappend` y@(GL [] 0) = x
  x@(GL xs tf1) `mappend` y@(GL ys tf2)= GL (xs++ys) (tf1+tf2)

instance Semigroup GuestList where
  (GL [] x) <> (GL [] y) = GL [] (x+y)
  (GL [] x) <> (GL xs y) = GL xs (x+y)
  (GL xs x) <> (GL [] y) = GL xs (x+y)
  (GL xs x) <> (GL ys y) = GL (xs ++ ys) (x+y)
