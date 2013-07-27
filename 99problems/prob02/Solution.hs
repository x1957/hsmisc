
module Solution where 

myButLast (x:[_]) = x
myButLast (_:y) = myButLast y


