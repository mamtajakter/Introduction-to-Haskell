{-# LANGUAGE FlexibleInstances #-}
module Calc where

-- Homework 5

import ExprT
import Parser
import qualified Data.Map as M

newtype MinMax = MinMax Integer deriving (Show, Eq, Ord)
newtype Mod7   = Mod7   Integer deriving (Show, Eq)


eval :: ExprT-> Integer
eval (Lit a)= a
eval (Add a b)= case (a,b) of
                (Lit x, Add y z)  -> x + (eval (Add y z))
                (Lit x, Mul y z)  -> x+ (eval (Mul y z))
                (Add y z, Lit x)  -> (eval (Add y z)) + x
                ( Mul y z, Lit x)  -> (eval (Mul y z)) + x
                (Lit x,Lit y)      -> x+y
eval (Mul a b)= case (a,b) of
                (Lit x, Add y z)  -> x * (eval (Add y z))
                (Lit x, Mul y z)  -> x* (eval (Mul y z))
                (Add y z, Lit x)  -> (eval (Add y z)) * x
                (Mul y z, Lit x)  -> (eval (Mul y z)) * x
                (Lit x,Lit y)      -> x*y

evalStr:: String-> Maybe Integer
evalStr x= case y of
        Nothing -> Nothing
        Just n-> Just (eval n)
       where y=parseExp Lit Add Mul x


class Expr a where
    lit :: Integer-> a
    add :: a -> a -> a
    mul :: a -> a -> a


instance Expr ExprT where
   lit x = Lit x
   add a b = case (a,b) of
          (Lit x, Add y z)  ->  add (Lit x) (add y z)
          (Lit x, Mul y z)  ->  add (Lit x) (mul y z)
          (Add y z, Lit x)  -> add (add y z) (Lit x)
          ( Mul y z, Lit x)  -> add (mul y z) (Lit x)
          (Lit x,Lit y)      -> Lit (x+y)
   mul a b = case (a,b) of
          (Lit x, Add y z)  ->  mul (Lit x) (add y z)
          (Lit x, Mul y z)  ->  mul (Lit x) (mul y z)
          (Add y z, Lit x)  ->  mul (add y z) (Lit x)
          ( Mul y z, Lit x)  -> mul (mul y z) (Lit x)
          (Lit x,Lit y)      -> Lit (x*y)

instance Expr Integer where
   lit x= x
   add a b =   a  + b
   mul a b = a * b

instance Expr Bool where
   lit x
       | x<=0  = False
       | otherwise = True
   add a b= a || b
   mul a b = a && b


instance Expr MinMax where
   lit a = MinMax a
   add (MinMax a) (MinMax b)=  MinMax (max a b)
   mul (MinMax a) (MinMax b)=  MinMax (min a b)


instance Expr Mod7 where
   lit x = Mod7 x
   add (Mod7 a) (Mod7 b)= Mod7 ((a+b) `mod` 7)
   mul (Mod7 a) (Mod7 b)= Mod7 ((a*b) `mod` 7)

{-
instance Expr Program where
   lit x = (PushI x) :[]
   add (PushI a) (PushI b)= PushB (a+b) :[]
   mul (PushI a) (PushI b)= PushB (a*b) :[]
-}

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * 4) + 5"


testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
--testInteger = parseExp lit add mul "(3 * 4) + 5"


class HasVars a where
    var :: String-> a


data VarExprT = VarLit Integer
           | VarAdd VarExprT VarExprT
           | VarMul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)



instance Expr VarExprT where
   lit x = VarLit x
   add a b = case (a,b) of
          (VarLit x, VarAdd y z)  ->  add (VarLit x) (add y z)
          (VarLit x, VarMul y z)  ->  add (VarLit x) (mul y z)
          (VarAdd y z, VarLit x)  -> add (add y z) (VarLit x)
          ( VarMul y z, VarLit x)  -> add (mul y z) (VarLit x)
          (VarLit x,VarLit y)      -> VarLit (x+y)
          (VarLit x,Var y)      -> Var ( show x ++ "+"++ y)
          (Var x,VarLit y)      -> Var (  x ++ "+" ++  show y)
   mul a b = case (a,b) of
          (VarLit x, VarAdd y z)  ->  mul (VarLit x) (add y z)
          (VarLit x, VarMul y z)  ->  mul (VarLit x) (mul y z)
          (VarAdd y z, VarLit x)  -> mul (add y z) (VarLit x)
          ( VarMul y z, VarLit x)  -> mul (mul y z) (VarLit x)
          (VarLit x,VarLit y)      -> VarLit (x*y)
          (VarLit x,Var y)      -> Var ( show x ++ "*"++ y)
          (Var x,VarLit y)      -> Var (  x ++ "*" ++  show y)

instance HasVars VarExprT where
   var s= Var s

type Name =String
type PhoneNum= String
type PhoneBook =M.Map Name PhoneNum
