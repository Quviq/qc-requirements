{-# LANGUAGE TypeFamilies,
    	     TemplateHaskell,
	     LambdaCase
#-}

module NamedReqs where

import NameTree
import Logical
import Language.Haskell.TH
import Test.QuickCheck

newtype Requirement a = Requirement { unRequirement :: a -> Named (Covered Position Bool) }

instance Show (Requirement a) where
  show (Requirement f) = show . assignNames . f $ error "show: Requirement is not a constant"

instance Logical (Requirement a) where
  boolean b = atom $ \case (Position Nothing) -> boolean b
                           pos                -> withCover pos (boolean b)
  Requirement f #&& Requirement g = Requirement $ \a -> (#&&) <$> f a <*> g a
  Requirement f #|| Requirement g = Requirement $ \a -> (#||) <$> f a <*> g a
  Requirement f #>&& Requirement g = Requirement $ \a -> (#>&&) <$> f a <*> g a
  Requirement f #>|| Requirement g = Requirement $ \a -> (#>||) <$> f a <*> g a
  holds (Requirement f) = holds . decision . snd . assignNames . f $ (error "holds: Requirement is not constant")
  b #=> Requirement r = Requirement $ \a -> do
    ok <- r a
    pure (if b then ok else boolean True)

instance Naming (Requirement a) where
  type NamedType (Requirement a) = Covered Position Bool
  atom f = Requirement $ \_ -> atom f
  named s = liftToRequirement $ named s
  group numeral = liftToRequirement $ group numeral
  anonymous = liftToRequirement anonymous

instance Erroring (Requirement a) where
  withError msg = liftToRequirement $ fmap (withError msg)

liftToRequirement f (Requirement g) = Requirement (f . g)

withValue f = Requirement $ \a -> unRequirement (f a) a

onValue (Requirement r) a = Requirement $ \_ -> r a

-- The requirement matching [| \pat -> req |] holds if the requirement
-- parameter matches pat, and requirement req holds. If the parameter
-- does not match pat then the requirement is trivially satisfied (but
-- not covered).
matching exp = do
  e <- exp
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

requirementHolds (Requirement f) =
  foldr (.) id [cover 1 (pos `elem` covered b) (show pos) | pos <- positions] $
  counterexample ("Failed requirements: "++show (failed b)) $
  foldr (.) id [counterexample err | err <- errors b] $
  property $ decision b
  where (positions, b) = assignNames . f $ error "requirementHolds: Requirement is not a constant"

requirementAttacks (Requirement f) =
  foldr (.) id [cover 1 (not (decision b) && pos `elem` covered b) (show pos)
               | pos <- positions] $
  classify (not (decision b)) "Successful attacks" $
  True
  where (positions, b) = assignNames . f $ error "requirementHolds: Requirement is not a constant"

