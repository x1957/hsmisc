import Data.Char

strong :: [Char] -> Bool
strong s = (length s >= 15)&&(hasUp s)&&(hasLow s)&&(hasDigit s)
	where
		hasUp s = foldl (\acc x-> acc || (isUpper x)) False s
		hasLow s =  foldl (\acc x-> acc || (isLower x)) False s
		hasDigit s = foldl (\acc x-> acc || (isNumber x)) False s