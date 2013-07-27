
module Solution where


indp p n = if mod n p == 0 then 1 + indp p (div n p) else 0

filp p n = if mod n p == 0 then filp p (div n p) else n

minfac n = 
    let m = head [k | k <- [2..], k * k > n] - 1
    in  let d = [k | k <- [2 .. m], mod n k == 0]
        in  if null d then n else head d

factors 1 = []
factors n = 
    let p = minfac n
    in  p : factors (filp p n)

totient n = foldl (\x -> \y -> div x y * (y - 1)) n (factors n)

