
module Solution where

elementAt (x:_) 1 = x
elementAt (_:y) k = elementAt y (k - 1)

