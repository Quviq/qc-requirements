module Numerals(arabic, letter, capitals, roman) where

arabic, letter, capitals, roman :: (Show a, Integral a) => a -> String
arabic = show
letter = numeral ['a'..'z']
capitals = numeral ['A'..'Z']

numeral xs i = loop (fromIntegral (i-1))
  where loop i | i < base = [xs!!i]
               | otherwise = loop (i `div` base)++[xs !! (i `mod` base)]
        base = length xs
  
roman n | 0 < n && n < 4000 = toRoman (fromIntegral n)
        | otherwise         = error $ "No roman numeral for "++show n
  where toRoman n | n < 10   = romanDigit 'i' 'v' 'x' n
	          | n < 100  = romanDigit 'x' 'l' 'c' (n `div` 10)  ++ toRoman (n `rem` 10)
		  | n < 1000 = romanDigit 'c' 'd' 'm' (n `div` 100) ++ toRoman (n `rem` 100)
		  | n < 4000 = romanDigit 'm'  u   u  (n `div` 1000)++ toRoman (n `rem` 1000)
          where u = error "Undefined roman numeral"
	romanDigit unit five ten n | n <  4 = replicate n unit
		   	     	   | n == 4 = [unit, five]
				   | n < 9  = five:replicate (n-5) unit
				   | n == 9 = [unit, ten]
