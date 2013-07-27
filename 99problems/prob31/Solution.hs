

module Solution where

isPrime 1 = False
isPrime 2 = True
isPrime n = length [ d | d <- [2 .. m], mod n d == 0] == 0
    where m = (head [ k | k <- [1 .. ], k * k > n]) - 1

