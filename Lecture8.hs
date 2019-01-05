

import Data.Char
import Control.Monad
import System.IO
import System.Environment

main0= putStrLn "Hello 34World1"


main1 = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")


main2 = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Read this carefully, because this is your future: \n" ++ tellFortune name

tellFortune x= "You will become a renowned researcher someday, " ++ x


main3 = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")




main4 = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"



main5 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main5

reverseWords :: String -> String
reverseWords = unwords . map reverse . words


main6 = do
    return ()
    return "HAHAHA"
    line <- getLine
    let b="Ohana"
    a<-return "BLAH BLAH BLAH"
    return 4
    putStrLn line
    putStrLn a
    putStrLn b



main7 = do  print True
            print 2
            print "haha"
            print 3.2
            print [3,4,3]



main8 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main8
        else return ()




main9 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main


main10 = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

main11 = do
     a<- sequence [getLine, getLine, getLine]
     print a





main13= do
     colors <- forM [1,2,3,4] (\a-> do
          putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
          color<- getLine
          return color)
     putStrLn "The colors that you associate with 1,2,3 and 4 are: "
     mapM_ putStrLn colors



main14 = forever $ do
    l <- getLine
    putStrLn $ map toUpper l

main15 = do
    l <- getContents
    putStrLn $ map toUpper l

main16= do
  contents <- getContents
  putStr (shortLinesOnly contents)

main17= interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
  let a= lines input
      s= filter (\x-> length x <10) a
      x= unlines s
  in x



--palindromeOnly :: String -> String

main18= interact $ unlines . filter ((<10) . length) . lines


main19 = do
    line <- getLine
    if reverse line == line
        then do
            putStrLn $ "Hoohoo! Its a palindrome!!"
        else do
            putStrLn "No, try again:"
            main

main20= interact palindromeOnly

palindromeOnly :: String -> String
palindromeOnly= unlines . map (\x-> if x==reverse x then "Palindrome" else "Not a Palindroe"  ) . lines

main21 = do
    handle <- openFile "haiku.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle


main22 = do
   withFile "haiku.txt" ReadMode (\handle-> do
                   contents<- hGetContents handle
                   putStrLn contents )


withFile' :: FilePath-> IOMode -> (Handle-> IO a)-> IO a
withFile' path mode getcontents= do
      handle<- openFile path mode
      contents<- getcontents handle
      hClose handle
      return contents


main23 = do
   withFile' "haiku.txt" ReadMode (\handle-> do
                   contents<- hGetContents handle
                   putStrLn contents )

main24= do
   contents<- readFile "haiku.txt"
   putStr contents

main25= do
   contents<- readFile "haiku.txt"
   writeFile "grlfriend.txt" (map toUpper contents)


main26 = do
   args <- getArgs
   progName <- getProgName
   putStrLn "The arguments are:"
   mapM_ putStrLn args
   putStrLn "The program name is:"
   putStrLn progName

main27 = putStrLn "Hello" >> putStrLn "World"


--main = putStrLn "Hello" >> putStrLn "world!"
main :: IO ()
main = (return "hello" >> return "world") >>= putStrLn
