
module Solution where

rotate l k = 
    let m = length l
        n = mod (k + m) m
    in  drop n l ++ take n l
