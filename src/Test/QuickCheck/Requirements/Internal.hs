{-# LANGUAGE TypeFamilies,
    	     TemplateHaskell,
	     LambdaCase
#-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Test.QuickCheck.Requirements.Internal(
  Requirement,
  matching,
  onValue,
  recursively,
  requirementHolds,
  requirementAttacks,
  requirementChecked,
  requirementCovered
  ) where

import Test.QuickCheck.Requirements.NameTree
import Test.QuickCheck.Requirements.Logical
import Language.Haskell.TH
import Test.QuickCheck

newtype Requirement a = Requirement { unRequirement :: a -> Named (Covered Position Bool) }

instance Show (Requirement a) where
  show (Requirement f) = show . assignNames . f $ error "show: Requirement coverage depends on the input"

instance Logical (Requirement a) where
  boolean b = atom $ \case (Position Nothing) -> boolean b
                           pos                -> withCover pos (boolean b)
  Requirement f #&& Requirement g = Requirement $ \a -> (#&&) <$> f a <*> g a
  Requirement f #|| Requirement g = Requirement $ \a -> (#||) <$> f a <*> g a
  Requirement f #>&& Requirement g = Requirement $ \a -> (#>&&) <$> f a <*> g a
  Requirement f #>|| Requirement g = Requirement $ \a -> (#>||) <$> f a <*> g a
  holds (Requirement f) = holds . decision . snd . assignNames . f $
                            (error "holds: Requirement is not constant")
  b #=> Requirement r = Requirement $ \a -> do
    ok <- r a
    pure (if b then ok else boolean True)
  negation = named "negation" . liftToRequirement (fmap negation)
  
instance Naming (Requirement a) where
  type NamedType (Requirement a) = Covered Position Bool
  atom f = Requirement $ \_ -> atom f
  named s = liftToRequirement $ named s
  group numeral = liftToRequirement $ group numeral
  anonymous = liftToRequirement anonymous

instance Erroring (Requirement a) where
  withError msg = liftToRequirement $ fmap (withError msg)

liftToRequirement :: (Named (Covered Position Bool) -> Named (Covered Position Bool))
                     -> Requirement a -> Requirement a
liftToRequirement f (Requirement g) = Requirement (f . g)

withValue :: (a -> Requirement a) -> Requirement a
withValue f = Requirement $ \a -> unRequirement (f a) a

onValue :: Requirement a -> a -> Requirement b
onValue (Requirement r) a = Requirement $ \_ -> r a

-- The requirement matching [| \pat -> req |] holds if the requirement
-- parameter matches pat, and requirement req holds. If the parameter
-- does not match pat then the requirement is trivially satisfied (but
-- not covered).
matching :: Q Exp -> Q Exp
matching expr = do
  e <- expr
  case e of
    LamE [a] body -> do
      [| withValue $ \ x ->
           case x of
             $(pure a) -> True
             _         -> False
           #=>
           let $(pure a) = x in $(pure body) |]

recursively :: (Requirement a -> Requirement a) -> Requirement a
recursively f = Requirement rec
  where rec = recursivelyNamed (\recurse a -> unRequirement (f (Requirement recurse)) a)

-- Testing requirements

-- The next two functions are intended for use when an implementation
-- maps an input to an output of the same type, which is guaranteed to
-- meet the requirements. For example, a filter function which filters
-- out bad transactions from a list. Thus *positive* testing is
-- applied to the output of the implementation, while *negative*
-- testing is applied to its input (make sure it is given inputs that
-- violate the requirements in every possible way). In each case the
-- requirement must not depend on input data.

-- Positive testing of requirements. Each position in the requirement
-- must be covered in at least cov% of tests.

requirementHolds :: Double -> Requirement a -> Property
requirementHolds cov (Requirement f) =
  foldr (.) id [cover cov (pos `elem` covered b) (show pos) | pos <- positions] $
  counterexample ("Failed requirements: "++show (failed b)) $
  foldr (.) id [counterexample err | err <- errors b] $
  property $ decision b
  where (positions, b) = assignNames . f $ error "requirementHolds: Requirement positions depend on input data"

-- Negative testing of requirements. Each position in the requirement
-- must be covered in at least cov% of tests.
requirementAttacks :: Double -> Requirement a -> Property
requirementAttacks cov (Requirement f) =
  foldr (.) id [cover cov (not (decision b) && pos `elem` covered b) (show pos)
               | pos <- positions] $
  classify (not (decision b)) "Successful attacks" $
  True
  where (positions, b) = assignNames . f $ error "requirementAttacks: Requirement positions depend on input data"


-- Check that a requirement corresponds to a predicate, and collect
-- positive and negative coverage for each position. Error if each
-- position is not covered by at least covPos% or covNeg% of tests.

requirementChecked :: Double -> Double -> Requirement t -> (t -> Bool) -> t -> Property
requirementChecked covPos covNeg (Requirement r) p x =
  foldr (.) id [cover covPos
                      (decision b && pos `elem` covered b)
                      (show pos) .
                cover covNeg
                      (not (decision b) && pos `elem` covered b)
                      (show (annot pos))
               | pos <- positions ] $
  p x === decision b
  where (positions,  b)  = assignNames (r x)
        annot (Position (Just pos)) = Position (Just ("-":pos))

-- Check that generated test data achieves both positive and negative
-- coverage of all positions in the requirement, in at least covPos%
-- and covNeg% of test cases. That is, this tests the test data. It
-- should be used with checkCoverage (although calling checkCoverage
-- here would make supplying a custom generator impossible).

requirementCovered :: Double -> Double -> Requirement a -> a -> Property
requirementCovered covPos covNeg (Requirement r) =
  -- Using a separate lambda here enables sharing of the positions computation.
  \x ->
  let (_,  b)  = assignNames (r x) in
  foldr (.) id [cover covPos
                      (decision b && pos `elem` covered b)
                      (show pos) .
                cover covNeg
                      (not (decision b) && pos `elem` covered b)
                      (show (annot pos))
               | pos <- positions ] $
  property True
  where annot (Position (Just pos)) = Position (Just ("-":pos))
        (positions, _) = assignNames . r $ error "requirementCovered: Requirement positions depend on input data"
