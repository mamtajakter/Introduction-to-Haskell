module Lecture2 where

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, Ship, SealingWax, Cabbage, King ]

isSmall :: Thing-> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King= False

isSmall2 :: Thing-> Bool
isSmall2 King= False
isSmall2 Ship = False
isSmall2 _  = True

data FailableDouble = Failure
                    | OK Double
      deriving Show

-- Failure is a value of type FailableDouble
-- (OK 3.6) is a value of type FailableDouble

e1= Failure
e2=OK 3.4

safeDiv :: Double-> Double-> FailableDouble
safeDiv _ 0= Failure
safeDiv x y= OK (x/y)



data Person = Person String Int Thing
      deriving Show

brent:: Person
brent= Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

steve :: Person
steve = Person "Steve" 105 Shoe

getAge :: Person-> Int
getAge (Person _ a _)=a

baz :: Person-> String
baz p@(Person n _ _)= "the name field of (" ++ show p  ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ Shoe)= n ++ ", you are my kind of person"
checkFav (Person n _ _ ) = n ++ ", not interested!"

e3= case "Hello" of
      []      -> 3
      ('H':s) -> length s
      _       -> 7


failureToZero :: FailableDouble -> Double
failureToZero Failure= 0
failureToZero (OK d)=d


failureToZero' :: FailableDouble -> Double
failureToZero' n= case n of
                     Failure -> 0
                     OK d    -> d

data IntList = Empty | Cons Int IntList

intListProd :: IntList-> Int
intListProd Empty= 1
intListProd (Cons a b)= a * intListProd b

data Tree= Leaf Char
         | Node Tree Int Tree
       deriving Show
tree :: Tree
tree= Node (Node (Leaf 'M') 3 (Leaf 'I')) 6 (Node (Leaf 'R') 9 (Leaf 'A'))
