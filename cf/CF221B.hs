-- CF221B

module Main where
import Data.List

ch2int c
    | c == '1' = 1
    | c == '2' = 2
    | c == '3' = 3
    | c == '4' = 4
    | c == '5' = 5
    | c == '6' = 6
    | c == '7' = 7
    | c == '8' = 8
    | c == '9' = 9
    | otherwise = 0

str2int [] = 0
str2int s = (str2int (init s)) * 10 + (ch2int (last s))

ex n b
    | n < b = [n]
    | otherwise = ex (div n b) b ++ [mod n b]

ex10 n = ex n 10

-- jo m n = intersect (ex m 10) (ex n 10)
jo m n = intersect (ex10 m) (ex10 n)

--cart (op, x, y) = [op a b | a <- x, b <- y]
cart x y = [a * b | a <- x, b <- y]

p n = head (filter (\m -> (mod n m) == 0) ([2..(floor (sqrt (fromIntegral n)))] ++ [n]))

v p n
    | (mod n p) == 0 = 1 + v p (div n p)
    | otherwise = 0

d n
    | n == 1 = [1]
    | otherwise = cart (d (div n ((p n)^(v (p n) n)))) (map ((p n)^) [0..(v (p n) n)])

ep n = filter (\m -> not (null (jo m n))) (d n)

lep n = length (ep n)

main = do
    n <- getLine
    print (length (ep (str2int n)))
