module Dummy where
import Prelude (undefined, Show, Eq, Ord, Bool)
import MyLogic
 
f :: List a -> List a
 
g :: List a -> List a
g = undefined
 
head :: List a -> a
 
tail :: List a -> List a
head (Cons (x_11_11, x_11_12)) = x_11_11
tail (Cons (x_11_11, x_11_12)) = x_11_12
 
data List a = Nil
            | Cons !(a, List a)
            deriving (Show, Eq, Ord)
f x = x
