module Lecture1 where


x:: Int
x=3 -- x is defined to be 3

i :: Int
i= -78

lb, hb :: Int
lb=maxBound
hb=minBound

n:: Integer
n=4547587687987687567454342342565786878755634534523425665756576546545646545646755675657

tooBig:: Integer
tooBig= 2^2^2^2^2

numDig :: Int
numDig = length (show tooBig)


d1, d2 :: Double
d1= 4.5387
d2= 3.435435e-4

f1, f2 :: Float
f1= 4.5387
f2= 3.43543e-4


b1,b2 :: Bool
b1= True
b2=False

c1,c2,c3:: Char
c1='x'
c2= '#'
c3='$'


s1:: String
s1="hello"


e1 = 3+2
e2=19-23
e3=2.45*3.89
e4=8.7/3.1
e5=mod 19 3
e6=19 `mod` 3
e7= 7^22
e8=(-3)*(-8)

badA = e1 +e3

j=3.543
bad2= j/j  -- / takes two floats

e9= i `div` 2  -- `div` takes two int/integer

e10= 12 `div` 5

--e11= (fromIntegral i) / (fromIntegral i)

--ii=3
--e12= ii / ii


e11=True && False

e12=not (False || True)

e13= ('a'=='a')

e14=(16 /=3)

e15 = (5>3) && ('p'<='q')

e16= "Haskell">"C++"


sums :: Int-> Int
sums 0 = 0
sums n= n+ sums (n-1)


foo :: Int-> Int
foo 0= 16
foo 1
    | "Haskell" > "C++" = 3
    | otherwise         = 4
foo n
    | n<0             = 0
    | n `mod` 17 == 2 = -43
    | otherwise       = n+3

isEven :: Int-> Bool
isEven n
      | n `mod` 2 ==0 = True
      | otherwise     = False

p:: (Int, Char)
p= (3,'s')

sump :: (Int, Int) -> Int
sump (x,y)= x+y

f:: Int -> Int -> Int-> Int
f x y z= x+y+z

e17 :: Int-> Int
e17 n= f 1 (n+1) 3

num, r1, r2 :: [Int]
num= [1,2,4,5,4]
r1= [1..100]
r2= [2, 4..100]

h1 :: [Char]
h1=['h', 'e','l','l','o']

h2:: String
h2= "hello"

hellosame= h1==h2
e18=1:[]
e19= 3: (1: [])
e20=2:3:4:[]

hailstone :: Int -> Int
hailstone n
      | n `mod` 2 ==0 = n `div` 2
      | otherwise     = 3*n +1


hailstoneSeq :: Int-> [Int]
hailstoneSeq 1 = [1]
hailstoneSeq n = n: hailstoneSeq (hailstone n)


intListLength  :: [Int]-> Int
intListLength []=0
intListLength (_:xs)= 1+ intListLength xs

sumEveryTwo :: [Int]-> [Int]
sumEveryTwo []=[]
sumEveryTwo (x:[])= [x]
sumEveryTwo (x:y:xs)=(x+y): sumEveryTwo xs


hailstoneLen :: Int-> Int
hailstoneLen n= intListLength (hailstoneSeq n) -1
