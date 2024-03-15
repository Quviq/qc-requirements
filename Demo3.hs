{-# LANGUAGE TemplateHaskell #-}

module Demo3 where

import NameTree
import Logical
import Numerals
import NamedReqs
import Test.QuickCheck
import Control.Monad
import Data.Either

multiply m n
  | 0 <= m && 0 <= n && m <= 100 && n <= 100 = Right $ m*n
  | otherwise = Left $ "Bad multiply " ++ show m ++ " " ++ show n

prop_multiply gen =
  forAllShrink (liftM2 (,) gen gen) shrink $ \(m,n) ->
  requirementHolds $ multiplyReq `onValue` (m,n,multiply m n)

prop_multiplyAttacks gen =
  forAllShrink (liftM2 (,) gen gen) shrink $ \(m,n) ->
  forAll (oneof [arbitrary, pure (Right (m*n))]) $ \res ->
  collect (head (words $ show res)) $
  requirementAttacks $ multiplyReq `onValue` (m,n,res)

multiplyReq :: Requirement (Int,Int,Either String Int)
multiplyReq =
  $(matching [| \(m,n,Right mn) ->
    named "m" (inRange `onValue` m) #&&
    named "n" (inRange `onValue` n) #&&
    named "result" (boolean (mn == m*n))
  |]) #&&
  $(matching [| \(m,n,Left _) ->
    named "failure-case" $
    named "m" (negation inRange `onValue` m) #|| named "n" (negation inRange `onValue` n)
  |])

inRange :: Requirement Int
inRange =
  $(matching [| \n ->
      named "inRange" . group roman $
          (named "lowerBound" $ n `ge` 0)
      #&& (named "upperBound" . withError (show n ++" > 100") $ 100 `ge` n)
  |])

m `ge` n =
  named "boundary"     (boolean (m==n)) #||
  named "non-boundary" (boolean (m >n))

mult (m,n,_) = (m,n,multiply m n)

testCaseGen :: Gen (Int,Int,Either String Int)
testCaseGen = do
  let valGen = --choose (-10,110) -- with this generator, coverage checking fails.
               oneof [elements [0,100], choose (-10,110)]
  m <- valGen
  n <- valGen
  res <- oneof [arbitrary, pure $ Right (m*n)]
  pure (m,n,res)

multPred (m,n,res) = case multiply m n of
  Left _ -> isLeft res
  right  -> res == right

--quickCheck . checkCoverage $ forAllShrink testCaseGen shrink $ requirementChecked multiplyReq multPred