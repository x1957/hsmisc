--PAT1001
--http://pat.zju.edu.cn/contests/pat-practise/1001
--Date 2012-10-20
getIntPairs :: IO (Int,Int)
getIntPairs = do ns <- getLine
                 return (make_pair [(read n::Int)|n<-(words ns)])
                        where
                          make_pair (x:y:_) = (x,y)
                          make_pair _ = (0,0)

addComma xs = 
	inFun xs 0 []
	where 
		inFun [] _ list = list 
		inFun ['-']  _ list = list ++ ['-']
		inFun (x:xs) len list = if (len > 0 && len `mod` 3 == 0)  
			then inFun xs (len + 1) (list++[',',x])
			else inFun xs (len + 1) (list++[x])

main = do
	y <- getIntPairs
	let a = fst y
	let b = snd y
	putStrLn $ reverse $ addComma $ reverse $ show (a + b)