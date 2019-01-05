module Lecture10 where

import Control.Applicative

type Name= String

data Employee = Employee { name :: Name, phone :: String}
           deriving Show

ex1 = liftA2 Employee (Just "Brent") (Just "6789")
ex2 = liftA2 Employee (Nothing) (Just "3467")
ex3 = liftA2 Employee (Just "Billy") (Nothing)
ex4 = liftA2 Employee (Nothing) (Nothing)


fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
fmap2 h fa fb = undefined

m_name1, m_name2 :: Maybe Name
m_name1= Nothing
m_name2= Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2= Just "97687"

ex01= Employee<$> m_name1 <*> m_phone1
ex02= Employee<$> m_name1 <*> m_phone2
ex03= Employee<$> m_name2 <*> m_phone1
ex04= Employee<$> m_name2 <*> m_phone2
