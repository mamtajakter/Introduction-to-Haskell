module Single where


import Data.List
-- Homework 4


--1--
fun1 :: [Integer]-> Integer
fun1 []= 1
fun1 (x:xs)
         | even x     = (x-2)* fun1 xs
         | otherwise  = fun1 xs

fun1' :: [Integer]-> Integer
fun1' = product . map (\x -> x-2) . filter even


fun2 :: Integer -> Integer
fun2 1 =0
fun2 n
     | even n    = n+ fun2 (n `div` 2)
     | otherwise = fun2 (3*n + 1)


fun2' :: Integer-> Integer
fun2' = sum . filter even .takeWhile (>1) . iterate (\x-> case (even x)  of
                                              True ->  (x `div` 2)
                                              False-> 3*x +1)



--2--

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
        deriving Show

inserts :: a -> Tree a-> Tree a
inserts x Leaf =  Node 0 Leaf x Leaf
inserts x t@(Node n l@Leaf y r)= Node 1 (inserts x l) y r
inserts x t@(Node n l y r@Leaf)= Node 1 l y (inserts x r)
inserts x t@(Node n l@(Node _ _ _ _) y r@(Node _ _ _ _))= Node (n+1)  (inserts x l) y r

foldTree :: [a]-> Tree a
foldTree []=Leaf
foldTree (x:[])=Node 0 Leaf x Leaf
foldTree (x:xs)= createTree (Node 0 Leaf x Leaf) xs
              where
                  createTree tr []=tr
                  createTree tr (t:ts) = createTree (inserts t tr) ts

--3--

--xor :: [Bool] -> Bool
--xor = odd . length . filter (== True)

xor :: [Bool] -> Bool
xor xs= odd $ foldr (\x acc-> if x==True then (acc+1) else acc) 0 xs

map' :: (a->b)-> [a]->[b]
map' f xs= foldr (\x acc-> f x : acc) [] xs

--3 Optional--

myFoldl' :: (a->b->a)-> a -> [b]-> a
myFoldl' f base xs= foldr (\x acc -> f acc x) base xs



--4--

sieveSundaram :: Integer-> [Integer]
sieveSundaram n= map (\x-> 2*x +1) $ filter (\x-> x `notElem` ys) xs
             where xs=[1..n]
                   ys=filter (<n) [ x+y+2*x*y | x<-[1..n] , y<-[1..n] , x<=y ]


cartProd :: Integer -> [Integer]
{- The sequence to be excluded from the [1..n] list later.
   1 <= i <= j and i + j + 2ij <= n. -}
cartProd n = filter (<= n) $ [i + j + 2*i*j | j <- [1..n], i <- [1..j]]


sieveSundaram' :: Integer -> [Integer]
{- The sieve that generates odd primes less than 2n + 2. -}
sieveSundaram' n = (map ((+1) . (2*) . head) . filter (\x -> length x == 1) .
                   group . sort . ([1..n] ++) . cartProd) n


--cartProd 5 =[4]
--cartProd 10= [4,7,10]
-- [1,2,3,4,5,6,7,8,9,10] ++ [4,7,10]
