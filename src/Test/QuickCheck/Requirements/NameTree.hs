{-# LANGUAGE TupleSections,
    	     TypeFamilies
#-}

module Test.QuickCheck.Requirements.NameTree(
  Position(..),
  Named,
  Naming(..),
  assignNames,
  recursivelyNamed) where

import Control.Monad
import Control.Monad.State
import Data.List hiding (group)

data LocalNameNode = LNGroup (Int -> String) LocalNames | LNNamed String LocalNames | LNAtom

type LocalNames = Sequence LocalNameNode

data GlobalNameNode = GNGroup GlobalNames | GNNamed GlobalNames | GNAtom Position
  deriving Show

type GlobalNames = Sequence GlobalNameNode

data Sequence a = Nil | Singleton a | Concat (Sequence a) (Sequence a)
  deriving Show

toList :: Sequence a -> [a]
toList s = toList' s []
  where toList' Nil = id
        toList' (Singleton x) = (x:)
        toList' (Concat s' s'') = toList' s' . toList' s''

numberSequence :: Sequence a -> Sequence (a, Int)
numberSequence s = fst (runState (number s) 1)
  where number Nil = pure Nil
        number (Singleton x) = Singleton . (x,) <$> incr
        number (Concat s' s'') = Concat <$> number s' <*> number s''
        incr = do i <- get
                  put (i+1)
                  pure i

instance Functor Sequence where
  fmap _ Nil = Nil
  fmap f (Singleton x) = Singleton (f x)
  fmap f (Concat s s') = Concat (fmap f s) (fmap f s')

fold :: (t -> t -> t) -> t -> Sequence t -> t
fold _  z Nil = z
fold _  _ (Singleton x) = x
fold op z (Concat s s') = fold op z s `op` fold op z s'

newtype Position = Position (Maybe [String])
 deriving (Eq, Ord)

instance Show Position where
  show (Position Nothing)   = "?"
  show (Position (Just [])) = "*"
  show (Position (Just pos)) =
    concat $ intersperse "." pos

-- Assign atoms a name based on their position in the hierarchy, with
-- the following optimisations:
--   - Groups of size 1 do not contribute a "1" to the positions.
--   - Group indices are dropped if followed immediately by a name unique in that group.
makeGlobalNames :: LocalNames -> GlobalNames
makeGlobalNames locals = make' [] show locals
  where make' _pos _numeral Nil  = Nil
          -- the next two lines ensure the "singleton" case kicks in when it should.
        make' pos  numeral (Concat Nil xs) = Concat Nil (make' pos numeral xs)
        make' pos  numeral (Concat xs Nil) = Concat (make' pos numeral xs) Nil
        make' pos _numeral (Singleton x)   = Singleton (make'' pos x)
        make' pos  numeral xs              = fmap addNumber (numberSequence xs)
          where addNumber (x,i) =
                  let pos' = pos ++ [numeral i]
                      pos'' = case isNamed x of
                                Just s | s `notElem` delete s names -> pos -- unique name
                                _                                   -> pos'
                  in make'' pos'' x
                names = [n | Just n <- toList $ fmap isNamed xs]
        make'' pos LNAtom = GNAtom (Position (Just pos))
        make'' pos (LNNamed s locals') = GNNamed (make' (pos++[s]) show locals')
        make'' pos (LNGroup numeral locals') = GNGroup (make' pos numeral locals')

isNamed :: LocalNameNode -> Maybe String
isNamed (LNNamed s _)   = Just s
isNamed (LNGroup _ (Singleton n)) = isNamed n
isNamed _               = Nothing

atomicPositions :: GlobalNames -> [Position]
atomicPositions globals = aPs globals []
  where aP (GNAtom pos) = (pos:)
        aP (GNNamed globals') = aPs globals'
        aP (GNGroup globals') = aPs globals'
        aPs = fold (.) id . fmap aP

anonymize :: LocalNames -> GlobalNames
anonymize Nil = Nil
anonymize (Concat ln ln') = Concat (anonymize ln) (anonymize ln')
anonymize (Singleton ln) = Singleton (anonymize' ln)
  where anonymize' LNAtom = GNAtom (Position Nothing)
        anonymize' (LNNamed _ locals) = GNNamed (anonymize locals)
        anonymize' (LNGroup _ locals) = GNGroup (anonymize locals)
  
newtype Named a = Named (LocalNames, GlobalNames -> a)

instance Functor Named where
  fmap f (Named (local,g)) = Named (local,f . g)

instance Applicative Named where
  pure x = Named (Nil, const x)
  (<*>) = ap

instance Monad Named where
  return = pure

  -- when we use (>>=), h MUST be non-strict!
  Named (local,f) >>= h =
    Named (Concat local local',
           \(Concat global1 global2) -> let Named (_,g) = h (f global1) in g global2)
    where Named (local',_) = h (error "NameTree: names may not depend on dynamic values")
    

assignNames :: Named a -> ([Position], a)
assignNames (Named (locals,f)) = (atomicPositions globals,f globals)
  where globals = makeGlobalNames locals

class Naming n where
  type NamedType n
  atom      :: (Position -> NamedType n) -> n
  named     :: String -> n -> n
  group     :: (Int -> String) -> n -> n
  anonymous :: n -> n

instance Naming (Named a) where
  type NamedType (Named a) = a
  
  atom f = Named (Singleton LNAtom,
                  \(Singleton (GNAtom pos)) -> f pos)

  named s (Named (locals,g)) = Named (Singleton (LNNamed s locals),
                                      \(Singleton (GNNamed globals))->g globals)

  group numeral (Named (locals,g)) =
    Named ((Singleton (LNGroup numeral locals)),
           \(Singleton (GNGroup globals))-> g globals)

  anonymous (Named (locals,g)) = Named (Nil,\Nil -> g (anonymize locals))

instance Show a => Show (Named a) where
  show = show . assignNames

recursivelyNamed :: ((a -> Named b) -> (a -> Named b)) -> a -> Named b
recursivelyNamed f a = Named (locals,rec a)
  where Named (locals,_) = f (const $ Named (Nil, \Nil -> err)) err
        err = error "NameTree (recursively): names may not depend on dynamic values"
        rec a' globals = g globals
          where Named (_,g) = f (\a'' -> Named (Nil, \_ -> rec a'' globals)) a'