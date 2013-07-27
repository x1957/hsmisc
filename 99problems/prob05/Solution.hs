
module Solution where


myReverse [] = []
myReverse (x:y) = myReverse y ++ [x]


