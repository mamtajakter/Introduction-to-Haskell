module Single where
import Prelude

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


--fun2' :: Integer-> Integer
--fun2' = sum  . filter even . iterate (\x-> x `div` 2)


fun2' :: Integer-> Integer
fun2' = sum . takeWhile (>1) . iterate (\x-> case (even x)  of
                             True -> x+ (x `div` 2)
                             False-> 3*x +1)


fun2''' :: Integer-> Integer
fun2''' = sum . takeWhile (>1) . iterate (\x-> case (even x) of
                                                True ->  x `div` 2
                                                False-> 3* x +1)


fun2'' :: [Integer]-> Integer
fun2'' = product . map (\x -> x-2) . filter even


{-
fun2' :: Integer -> Integer
fun2' 1 =0
fun2' =
-}

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

xor :: [Bool] -> Bool
xor = odd . length . filter (== True)

map' :: (a-> b)-> [a]->[b]
map' f [] = []
map' f xs= foldr (\x xs-> f x : xs) [] xs

myFoldl :: (a->b->a)-> a -> [b]->a
myFoldl f base [] = base
myFoldl f base xs = foldr step id xs base
                where step x g a = g (f a x)

chpr n= takeWhile (>1) (iterate (\x->x-1) n)

checkprime:: Integer-> Bool
checkprime n= checkp x n
          where x=takeWhile (>1) (iterate (\x->x-1) n)

checkp [] _ = True
checkp (x:y:xs) n
            | n `mod` y ==0 = False
            | otherwise     =  True && (checkp xs n)
