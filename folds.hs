myfoldl f z [] = z
myfoldl f z (x:xs) = myfoldl f (f z x) xs

myfoldr f z [] = z
myfoldr f z (x:xs) = f x (myfoldr f z xs)

rev [] = []
rev (x:xs) = rev xs ++ [x]

fac acc 0 = acc
fac acc n = fac (acc*n) (n-1)

count n (x:xs) =
  if (x == '0') then count (n+1) xs
                     else n

main = do
  let ans = count 0 (rev (show (fac 1 1024)))
  putStrLn (show ans )
--  putStrLn "Hello , 1957"