
module Solution where

compress [] = []
compress (x:[]) = [x]
compress (x:y:z) = 
    let r = compress (y:z) 
    in if x == y then r else x : r


