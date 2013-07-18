
module Main where

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

mod_exp a n m = exp n
    where exp n = if n == 0
            then 1
            else
                let b = exp (div n 2)
                    c = mod (b * b) m
                in  if mod n 2 == 0
                    then c
                    else mod (a * c) m

pow_sum a n m = ps n
    where ps n = if n == 0
            then 1
            else if mod n 2 == 0
                 then mod (1 + a * ps (n - 1)) m
                 else let k = div n 2
                      in  mod ((1 + (mod_exp a (k + 1) m)) * (ps k)) m

sub [] = [[]]
sub (x:y) =
    let w = sub y
    in  w ++ [x:z | z <- w]

magic seq = 
    mod (sum (map f (zip seq [0..]))) 1000000007
        where f (c, i) = if c == '0' || c == '5'
                then mod_exp 2 i 1000000007
                else 0

solve str k =
    mod ((pow_sum a (k - 1) 1000000007) * q) 1000000007
        where
            n = length str
            q = (magic str)
            a = mod (mod_exp 2 n 1000000007) 1000000007

main = do
    str <- getLine
    line <- getLine
    print $ solve str (str2int line)
