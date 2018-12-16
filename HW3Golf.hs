module Golf where

import Data.List
import GHC.Base

-- Homework 3

--1--

skips :: [a]  -> [[a]]
skips xs = reverse ( h xs (length xs))
    where h [] _ = [[]]
          h xs 1 = (h' xs 1) :  []
          h xs k = (h' xs k) : h xs (k-1)

    --      h' "ABCD" 2
    --      "BD"  -- picks every second
    -- h' "ABCDEFG" 3
    --       "CF  -- picks every second
    --  h' "ABCDEFG" 1
       --- "ABCDEFG"
       
h' :: [a] -> Int -> [a]
h' xs n = h xs n
    where h [] _ = []
          h (x:xs) 1 = x: h xs n
          h (x:xs) k = h xs (k-1)

--2--

localMaxima :: [Integer]-> [Integer]
localMaxima []=[]
localMaxima [x]=[]
localMaxima (x:y:[])=[]
localMaxima (x:y:z:xs)
               | (y>x) && (y>z)= y : localMaxima (z:xs)
                | otherwise = localMaxima (y:z:xs)



--3--

printStar [] = ""
printStar (x:xs)
             | x>0 = "*" ++ printStar xs
             | otherwise = " "++ printStar xs

printAllStar [[]] =""
printAllStar [x] =printStar x
printAllStar (x:xs) = printStar x ++ "\n" ++ printAllStar xs


finalPrint []=""
finalPrint xs= printAllStar (reverse xs) ++ "\n==========\n0123456789\n"


--       putStr (finalPrint [[0,2,3,0,0,3],[0,1,2,0,0,2],[0,0,1,0,0,1],[0,0,0,0,0,0]])

--         *  *
--        **  *
--        **  *

--       ==========
--       0123456789




--  h [0,3,0,0,0,1,0,0,0,0]
--  [0,2,0,0,0,0,0,0,0,0]

h []= []
h (x:xs)
       | x>0       = (x-1) : h xs
       | otherwise = x : h xs

--h1 [0,3,0,0,0,1,0,0,0,0]
--[[0,2,0,0,0,0,0,0,0,0],[0,1,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0]]

h1 []=[[]]-- how to stop it
h1 xs= if ( (nub xs) /= [0])
       then  (h xs) : (h1 (h xs))
       else (h1 [])

      -- countElem [1,1,1,5] 1
    --   3
countElem :: [Integer]-> Integer-> Integer
countElem [] _  = 0
countElem (x:xs) y
            | x==y      = 1+ countElem xs y
            | otherwise = countElem xs y

          --   countAll [1,1,1,5]
          --   [0,3,0,0,0,1,0,0,0,0]

countAll :: [Integer] -> [Integer]
countAll xs = reverse (helper xs 9)
           where helper [] _ =[]
                 helper xs 0 = [(countElem xs 0)] ++ []
                 helper xs k = [(countElem xs  k)] ++ helper xs (k-1)



histogram:: [Integer]-> String
histogram []= ""
histogram (x:xs) =finalPrint ((countAll (x:xs) ) : (init (init (h1 (countAll (x:xs))))))


--putStr(histogram [1,4,5,4,6,6,3,4,2,4,9])

--     *
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789
