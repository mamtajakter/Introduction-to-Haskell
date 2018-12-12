module Golf where

import Data.List
import GHC.Base

-- Homework 3

skips :: [a]  -> [[a]]
skips xs = reverse ( helper xs (length xs))
    where helper [] _ = [[]]
          helper xs 1 = (pickEvery' xs 1) :  []
          helper xs k = (pickEvery' xs k) : helper xs (k-1)

pickEvery' :: [a] -> Int -> [a]
pickEvery' xs n = helper xs n
    where helper [] _ = []
          helper (x:xs) 1 = x: helper xs n
          helper (x:xs) k = helper xs (k-1)

localMaxima :: [Integer]-> [Integer]
localMaxima []=[]
localMaxima [x]=[]
localMaxima (x:y:[])=[]
localMaxima (x:y:z:xs)
               | (y>x) && (y>z)= y : localMaxima (z:xs)
                | otherwise = localMaxima (y:z:xs)


      --       putStr (finalPrint [[0,2,3,0,0,3],[0,1,2,0,0,2],[0,0,1,0,0,1],[0,0,0,0,0,0]])

      --         *  *
      --        **  *
      --        **  *

      --       ==========
--             0123456789

finalPrint []=""
finalPrint xs= printAllStar (reverse xs) ++ "\n==========\n0123456789\n"

printAllStar [[]] =""
printAllStar [x] =printStar x
printAllStar (x:xs) = printStar x ++ "\n" ++ printAllStar xs

printStar [] = ""
printStar (x:xs)
             | x>0 = "*" ++ printStar xs
             | otherwise = " "++ printStar xs



          --   countAll [1,1,1,5]
          --   [0,3,0,0,0,1,0,0,0,0]

countAll :: [Integer] -> [Integer]
countAll xs = reverse (helper xs 9)
           where helper [] _ =[]
                 helper xs 0 = [(countElem xs 0)] ++ []
                 helper xs k = [(countElem xs  k)] ++ helper xs (k-1)


--h1 [0,3,0,0,0,1,0,0,0,0]
--[[0,2,0,0,0,0,0,0,0,0],[0,1,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0]]

h1 []=[[]]-- how to stop it
h1 xs= if ( (nub xs) /= [0])
       then  (h xs) : (h1 (h xs))
       else (h1 [])

--  h [0,3,0,0,0,1,0,0,0,0]
--  [0,2,0,0,0,0,0,0,0,0]

h []= []
h (x:xs)
       | x>0       = (x-1) : h xs
       | otherwise = x : h xs


      -- countElem [1,1,1,5] 1
    --   3
countElem :: [Integer]-> Integer-> Integer
countElem [] _  = 0
countElem (x:xs) y
            | x==y      = 1+ countElem xs y
            | otherwise = countElem xs y

--printStar :: String->Integer-> String

histogram:: [Integer]-> String
histogram []= ""
histogram (x:xs) =finalPrint ((countAll (x:xs) ) : (init (init (h1 (countAll (x:xs))))))


--putStr(histogram [1,4,5,4,6,6,3,4,2,4,9])

--    *
--    *
--    * *
-- ******  *
-- ==========
--0123456789
