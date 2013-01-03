-- CF214A
module Main where

import Data.List
import Char

ch2int c = ord c - ord '0'

str2int [] = 0
str2int s = (str2int (init s)) * 10 + (ch2int (last s))

solve m n = length solutions where
    solutions = [(a, b) | a <- [0 .. limit], b <- [0 .. limit], a * a + b == m, a + b * b == n ] where
        limit = min m n

work args = solve (args !! 0) (args !! 1)

main = do
    line <- getLine
    (print . work) (map str2int (words line))