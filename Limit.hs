module Limit where

import Test.QuickCheck

converging a b = ns' ++ [b | last ns'/=b]
  where ns  = iterate (\c -> (b+c)/2) a
        ns' = nodups ns

nodups (x:y:xs) | x==y = [x]
nodups (x:xs)          = x : nodups xs

limit a b = elements (converging a b)

boundary b = oneof [limit (b-1) b, limit (b+1) b]