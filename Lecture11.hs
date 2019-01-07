{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lecture11 where

import Lecture10

import Control.Applicative hiding ((*>))




names=["Suie","Joyi","Maui"]
phones=["7856","2390","9876"]

{-
newtype ZipList a= ZipList { getZipList :: [a] }
           deriving (Eq, Show, Functor)

instance Applicative ZipList where
        pure = ZipList . repeat
        ZipList fs <*> ZipList xs= ZipList (zipWith ($) fs xs)
-}
employees1= Employee <$> names <*> phones


(.+) = liftA2 (+)
(.*) = liftA2 (*)
-- nondeterministic arithmetic
n = ([4,5] .* pure 2) .+ [6,1] -- [14,9,16,11]



employees2= getZipList $ Employee <$> ZipList names <*> ZipList phones

data BigRecord = BR {getName :: Name,
                     getSSN  :: String,
                     getSalary :: Integer,
                     getPhone :: String,
                     getLicensePlate :: String,
                     getNumSickDays :: Int}

r= BR "Brent" "XXX-XX-XXX4" 60000000 "555-1234" "JGX-55T3" 2

getEmp :: BigRecord->Employee
getEmp = Employee<$> getName <*> getPhone

ex001 = getEmp r


pair :: Applicative f=> f a-> f b-> f (a,b)
pair fa fb= (\x y-> (x, y)) <$> fa <*> fb


pair' :: Applicative f=> f a-> f b-> f (a,b)
pair' fa fb= (,) <$> fa <*> fb


pair'' :: Applicative f=> f a-> f b-> f (a,b)
pair'' fa fb= liftA2 (,) fa fb


pair''' :: Applicative f=> f a-> f b-> f (a,b)
pair''' = liftA2 (,)


(*>) :: Applicative f => f a -> f b -> f b
(*>) fa fb = (\_ -> id) <$> fa <*> fb


mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
