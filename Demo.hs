module Demo where

import Reqs
import Test.QuickCheck

inRange n =
  nameReq "inRange" $ groupReq roman $
      (nameReq "lowerBound" . boolReq $ n >= 0)
  #&& (nameReq "greaterBound" . boolReq $ n <= 100)

