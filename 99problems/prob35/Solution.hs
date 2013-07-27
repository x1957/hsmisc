
module Solution where

filp p n = if mod n p == 0 then filp p (div n p) else n

minfac n = 
    let m = head [k | k <- [2..], k * k > n] - 1
    in  let d = [k | k <- [2 .. m], mod n k == 0]
        in  if null d then n else head d

primeFactors 1 = []
primeFactors n = 
    let p = minfac n
    in  p : primeFactors (div n p)

