-- CF158A
module Main where

import Control.Monad
import Data.List
import Char

strToInt str = rec 0 str where
	rec acc str = if (null str) then acc else (rec (acc * 10 + ch2int (head str)) (tail str))
		where ch2int c = ord c - ord '0'

line2ints line = map strToInt (words line)

solve2 n k lst = length lst1
	where lst1 = filter (\x -> x >= (lst !! (k-1))) lst2
		where lst2 = filter (> 0) lst

solve1 n k = do
	inputs <- getLine
	print (solve2 n k (line2ints inputs))

solve args = do
	solve1 (head args) (last args)

main = do
	line <- getLine
	solve (line2ints line)