{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Test.QuickCheck.Requirements.Numerals(arabic, letter, capitals, roman) where

arabic, letter, capitals, roman :: (Show a, Integral a) => a -> String
arabic = show
letter = numeral ['a'..'z']
capitals = numeral ['A'..'Z']

numeral :: Integral a => [Char] -> a -> String

numeral xs i = loop (fromIntegral (i-1))
  where loop j | j < base = [xs!!j]
               | otherwise = loop (j `div` base)++[xs !! (j `mod` base)]
        base = length xs
  
roman n | 0 < n && n < 4000 = toRoman (fromIntegral n)
        | otherwise         = error $ "No roman numeral for "++show n
  where toRoman m | m < 10   = romanDigit 'i' 'v' 'x' m
                  | m < 100  = romanDigit 'x' 'l' 'c' (m `div` 10)  ++ toRoman (m `rem` 10)
                  | m < 1000 = romanDigit 'c' 'd' 'm' (m `div` 100) ++ toRoman (m `rem` 100)
                  | m < 4000 = romanDigit 'm'  u   u  (m `div` 1000)++ toRoman (m `rem` 1000)
          where u = error "Undefined roman numeral"
        romanDigit unit five ten m | m <  4 = replicate m unit
                                   | m == 4 = [unit, five]
                                   | m < 9  = five:replicate (m-5) unit
                                   | m == 9 = [unit, ten]
