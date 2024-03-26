{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module RangeDemo where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Requirements

inRange :: (Ord a, Show a) => (a, a) -> Requirement a
inRange (lo,hi) =
  $(matching [| \n ->
      named "inRange" . group roman $
          (named "lowerBound" $ n `ge` lo)
      #&& (named "upperBound" . withError (show n ++" > 100") $ hi `ge` n)
  |])

ge :: Ord a => a -> a -> Requirement b
m `ge` n =
  named "boundary"     (boolean (m==n)) #||
  named "non-boundary" (boolean (m >n))

--ge m n = boolean (m >= n)

--------------

three :: Gen a -> Gen (a,a,a)
three gen = liftM3 (,,) gen gen gen

threeInRange :: (Ord a, Show a) => (a,a) -> Requirement (a,a,a)
threeInRange (lo,hi) =
  $(matching [| \(a,b,c) ->
      (named "a" $ inRange (lo,hi) `onValue` a) #&&
      (named "b" $ inRange (lo,hi) `onValue` b) #&&
      (named "c" $ inRange (lo,hi) `onValue` c)
      |])

---------------

params :: [String]
params = [[char] | char <- ['a'..'z']++['A'..'Z']]

genParams :: Monad m => m a -> m [a]
genParams gen = sequence [gen | _ <- params]

paramsInRange :: (Ord a, Show a) => (a, a) -> Requirement [a]
paramsInRange (lo,hi) =
  $(matching [| \ns ->
      foldr1 (#&&)
        [ named p $ inRange (lo,hi) `onValue` n
        | (p,n) <- zipParams ns]
    |])

-- like zip params ns, but *lazy in ns*
zipParams :: [a] -> [([Char], a)]
zipParams ns = z params ns
  where z []     _  = []
        z (p:ps) ns = (p,head ns):z ps (tail ns)

--------------

corrupt :: Gen a -> [a] -> Gen [a]
corrupt gen ns =
  oneof [return ns,
         do i <- choose (0,length ns-1)
            x <- gen
            corrupt gen [if j==i then x else n | (j,n) <- zip [0..] ns]]

