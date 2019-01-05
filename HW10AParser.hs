module AParser where

import           Control.Applicative

import           Data.Char

import Data.List

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

sequenceA' :: (Applicative f)=> [f a]-> f [a]
sequenceA' []= pure []
sequenceA' (x:xs)= (:) <$> x <*> sequenceA' xs

sequenceA'' :: (Applicative f)=> [f a]-> f [a]
sequenceA'' xs= foldr (liftA2 (:)) (pure []) xs

--1

first :: (a->b)-> (a,c)-> (b,c)
first f p@(x,y)= (f x, y)

instance Functor Parser where
       fmap f p = Parser (\s-> case runParser p s of
                      Nothing -> Nothing
                      Just (a, st)-> Just (f a, st))

exercise1 = do
  print $ first toLower ('A', "BC")
  print $ runParser (satisfy isUpper) "ABC"
  print $ runParser (toLower <$> satisfy isUpper) "ABC"

--2

{-
p1 has a  type Parser {runParser :: Parser (a->b)-> String-> Maybe (a->b, String)
-}

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  p1 <*> p2 = Parser $ \s -> case runParser p1 s of
    Nothing -> Nothing
    Just (f, st) -> case (runParser p2 st) of
            Nothing-> Nothing
            Just (g, str)-> Just (f g, str)

--3
abParser:: Parser (Char, Char)
abParser = (\x y -> (x,y)) <$> char 'a' <*> char 'b'


abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'


intPair :: Parser [Integer]
intPair = (\x _ y -> [x,y]) <$> posInt <*> char ' ' <*> posInt


--4

instance Alternative Parser where
       empty = Parser (\s-> Nothing)
       p1 <|> p2 = Parser (\s -> runParser p1 s <|> runParser p2 s)
{-     p1 <|> p2 = Parser ( \s -> case runParser p1 s of
         Nothing -> case (runParser p2 s) of
                 Nothing-> Nothing
                 Just (g, str)-> Just (g, str)
         Just (f, st) -> Just (f, st))
-}


--5

intOrUppercase :: Parser ()
intOrUppercase = (const () <$>  (satisfy isUpper)) <|>  ( const () <$> posInt)
