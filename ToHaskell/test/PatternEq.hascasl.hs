module Dummy where
import Prelude (undefined, Show, Eq, Ord, Bool)
import MyLogic
 
data A_s = A_s
         deriving (Show, Eq, Ord)
 
data A_t = A_t
         deriving (Show, Eq, Ord)
 
a :: A_s
a = undefined
 
b :: A_s
 
c :: A_t
 
a_snd :: (A_s, A_t) -> A_t
 
x :: A_s
x = undefined
 
y :: A_t
y = undefined
a_snd (x, y) = y
b = a
c = a_snd (x, y)
