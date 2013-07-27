
module Solution where

isPalindrome lst = lst == rev lst
    where 
        rev [] = []
        rev (x:y) = rev y ++ [x]

