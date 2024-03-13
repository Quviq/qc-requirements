{-# LANGUAGE TemplateHaskell #-}

module Demo2 where

import Logical
import NamedReqs
import NameTree
import Numerals

inRange =
  $(matching [| \n ->
      named "inRange" . group roman $
          (named "lowerBound" . boolean $ n >= 0)
      #&& (named "upperBound" . withError (show n ++" > 100") $ boolean $ n <= 100)
  |])

allInRange =
 $(matching [| \(a,b,c) ->
 	         named "a" (inRange `onValue` a) #&&
		 named "b" (inRange `onValue` b) #&&
		 named "c" (inRange `onValue` c)
            |])

allElements r = recursively $ \rec ->
  $(matching [| \[] -> anonymous $ boolean True |]) #&&
  $(matching [| \(x:xs) -> (r `onValue` x) #&& (rec `onValue` xs) |])

-- This one contains infinitely many positions, of course.
allElements' r = 
  $(matching [| \[] -> named "emptyList" $ boolean True |]) #&&
  $(matching [| \(x:xs) -> (r `onValue` x) #&& (allElements' r `onValue` xs) |])
