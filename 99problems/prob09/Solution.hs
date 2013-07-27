
module Solution where


pack [] = []
pack (x:[]) = [[x]]
pack (x:y:z) = 
    let p = pack (y : z)
    in  if x == y
        then (x : (head p)) : (tail p)
        else [x] : p

