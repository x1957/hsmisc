module Misc.Plain(plain) where

data Plain = Plain String
instance Show Plain where show (Plain s) = s

plain s = Plain s
