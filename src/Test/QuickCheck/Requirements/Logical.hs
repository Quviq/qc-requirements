{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , TypeFamilies
#-}

module Test.QuickCheck.Requirements.Logical where

infixr 3 #&&, #>&&
infixr 2 #||, #>||
infixr 1 #=>

class Logical a where
  boolean :: Bool -> a
  (#&&), (#||), (#>&&), (#>||) :: a -> a -> a
  holds :: a -> Bool
  (#=>) :: Bool -> a -> a
  b #=> a = if b then a else boolean True
  negation :: a -> a

instance Logical Bool where
  boolean = id
  (#&&) = (&&)
  (#||) = (||)
  (#>&&) = (&&)
  (#>||) = (||)
  holds = id
  negation = not
  
data Covered pos a = Covered {
  covered  :: [pos],   -- if a covered position changes value, the decision will change
  failed   :: [pos],
  decision :: a,
  errors   :: [String]
  }
  deriving Show

instance (Logical a, Eq pos) => Logical (Covered pos a) where
  boolean b = Covered { covered  = [],
                        failed   = [],
                        decision = boolean b,
                        errors   = [] }

  c #&& c' = Covered {
    covered  = (if holds c' then covered c  else []) ++
               (if holds c  then covered c' else []),
    failed   = failed c ++ failed c',
    decision = decision c #&& decision c',
    errors   = errors c ++ errors c'
    }

  c #|| c' = Covered {
    covered  = (if holds c' then [] else covered c) ++
               (if holds c  then [] else covered c'),
    failed   = failed c ++ failed c',
    decision = decision c #|| decision c',
    errors   = errors c ++ errors c'
    }

  -- Lazy versions
  c #>&& c' = Covered {
    covered  = (if holds c' then covered c  else []) ++
               (if holds c  then covered c' else []),
    failed   = failed c ++
                   if holds c then failed c' else [],
    decision = decision c #>&& decision c',
    errors   = errors c ++
                   if holds c then errors c' else []
    }

  c #>|| c' = Covered {
    covered  = (if holds c' then [] else covered c) ++
               (if holds c  then [] else covered c'),
    failed   = failed c ++
                   if holds c then [] else failed c',
    decision = decision c #>|| decision c',
    errors   = errors c ++
               if holds c then [] else errors c'
    }
    
  holds c = holds (decision c)

  negation c = c {decision = negation (decision c)}

class Logical a => Covering a where
  type PositionType a
  withCover :: PositionType a -> a -> a

class Logical a => Erroring a where
  withError :: String -> a -> a

instance (Logical a, Eq pos) => Covering (Covered pos a) where
  type PositionType (Covered pos a) = pos
  
  withCover pos a = a {
    covered  = pos:covered a,
    failed   = if holds a then failed a else pos:failed a
    }

instance (Logical a, Eq pos) => Erroring (Covered pos a) where
  withError msg a
    | holds a   = a
    | otherwise = a { errors = errors a ++ [msg] }

