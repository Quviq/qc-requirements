{-# LANGUAGE TemplateHaskell #-}

module RangeDemo where

import Control.Monad
import Test.QuickCheck

import NamedReqs

inRange (lo,hi) =
  $(matching [| \n ->
      named "inRange" . group roman $
          (named "lowerBound" $ n `ge` lo)
      #&& (named "upperBound" . withError (show n ++" > 100") $ hi `ge` n)
  |])

m `ge` n =
  named "boundary"     (boolean (m==n)) #||
  named "non-boundary" (boolean (m >n))

--ge m n = boolean (m >= n)

--------------

three gen = liftM3 (,,) gen gen gen

threeInRange (lo,hi) =
  $(matching [| \(a,b,c) ->
      (named "a" $ inRange (lo,hi) `onValue` a) #&&
      (named "b" $ inRange (lo,hi) `onValue` b) #&&
      (named "c" $ inRange (lo,hi) `onValue` c)
      |])

---------------

params = [[char] | char <- ['a'..'z']++['A'..'Z']]

genParams gen = sequence [gen | _ <- params]

paramsInRange (lo,hi) =
  $(matching [| \ns ->
      foldr1 (#&&)
        [ named p $ inRange (lo,hi) `onValue` n
	| (p,n) <- zipParams ns]
    |])

-- like zip params ns, but *lazy in ns*
zipParams ns = z params ns
  where z []     _  = []
  	z (p:ps) ns = (p,head ns):z ps (tail ns)

--------------

corrupt gen ns =
  oneof [return ns,
  	 do i <- choose (0,length ns-1)
	    x <- gen
	    corrupt gen [if j==i then x else n | (j,n) <- zip [0..] ns]]

