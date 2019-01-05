module Lecture9 where


class Functor' f where
     fmap' :: (a->b)-> f a-> f b

instance Functor' Maybe where
     fmap' f Nothing= Nothing
     fmap' f (Just x)= Just (f x)

instance Functor' [] where
     fmap' f []=[]
     fmap' f (x:xs)= (f x) : fmap' f xs

instance Functor' IO where
     fmap' f ioa= do
             a<- ioa
             return (f a)
{-
instance Functor' IO where
     fmap' f ioa= ioa >>= (\a-> return (f a))

instance Functor' IO where
     fmap' f ioa= ioa >>= (return . f)


x
instance Functor' ((->) e) where
     fmap' f g= (\ee-> f ( g ee))
-}


instance Functor' ((->) e) where
     fmap' f g= f . g
