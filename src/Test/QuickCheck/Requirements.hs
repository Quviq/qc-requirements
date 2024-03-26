{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC "-fno-warn-unused-matches" #-}

module Test.QuickCheck.Requirements (
  module Test.QuickCheck.Requirements.Internal,
  module Test.QuickCheck.Requirements.Logical,
  module Test.QuickCheck.Requirements.NameTree,
  module Test.QuickCheck.Requirements.Numerals,
  allReq
  ) where

import Test.QuickCheck.Requirements.NameTree
import Test.QuickCheck.Requirements.Logical
import Test.QuickCheck.Requirements.Numerals
import Test.QuickCheck.Requirements.Internal

allReq :: Requirement a -> Requirement [a]
allReq r = recursively $ \rec ->
  $(matching [| \(x:xs) -> (r `onValue` x) #&& (rec `onValue` xs) |])
