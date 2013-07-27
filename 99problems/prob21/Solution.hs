
module Solution where

insertAt c l k = 
    let i = k - 1
    in  take i l ++ [c] ++ drop i l


