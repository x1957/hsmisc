
module Solution where

split l 0 = ([], l)
split (x:y) n = 
    let (z,w) = split y (n - 1)
    in  (x : z, w)

    
