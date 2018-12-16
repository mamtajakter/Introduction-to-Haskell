module LYAHchap6 where


multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

divideByTen :: (Floating a) => a -> a
divideByTen = (subtract 4.0) --infix function

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y= f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)


numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree'  = \x -> \y -> \z -> x + y + z


sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs


map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs


map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x] ) [] xs


maximum' :: Ord a => [a]-> a
maximum' xs = foldr1 (\x acc -> if x>acc then x else acc) xs


reverse' ::  [a]-> [a]
reverse'  = foldl (\acc x ->  x : acc) []

reverse'' ::  [a]-> [a]
reverse''  = foldl (flip (:)) []

--flip (:) (flip (:) (flip (:) (flip (:) [] 3) 4) 5) 6

product' :: Num a=> [a]->a
product' = foldr1 (\x acc ->  x * acc)


filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p xs= foldr (\x acc -> if p x then x : acc else acc) [] xs

head' :: [a] -> a
head' = foldr1 (\x acc ->  x)

last' :: [a]-> a
last' = foldl1 (\acc x ->  x)


--How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (\x acc -> acc+x)  (map sqrt [1..])))+1


--length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x) 
