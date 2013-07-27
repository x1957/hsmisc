
module Solution where

encode [] = []
encode (x:[]) = [(1, x)]
encode (x:y:z) = 
    let (c, t):r = encode (y : z)
    in  if x == y
        then (c + 1, t) : r
        else (1, x) : (c, t) : r

data Code a = Single a | Multiple Int a
    deriving Show

encodeModified lst = map f (encode lst)
    where f (c, t) = case c of
            1           -> Single t
            otherwise   -> Multiple c t
