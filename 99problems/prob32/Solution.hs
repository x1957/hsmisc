
module Solution where

myGCD a b = if b == 0 then abs a else myGCD b (mod a b)

