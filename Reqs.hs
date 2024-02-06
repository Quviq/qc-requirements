module Reqs where

import Data.List

newtype Position = Position [String]
 deriving (Eq, Ord)

instance Show Position where
  show (Position []) = "*"
  show (Position pos) =
    concat $ intersperse "." pos

---------

-- Order of defining things:
--  - First, we compute the number of branches within a requirement (either a boolean
--    requirement or a group or a named requirement).
--  - Then we can compute for each subtree what its child number is.
--  - Then we can determine the labels.

data StaticReq = StaticReq { covered  :: [Position], -- may not depend on dynamic data
     	       	 	     width    :: Int,        -- may not depend on dynamic data
     	       	 	     dynamic  :: DynamicReq
			   }
  deriving Show

data DynamicReq = DynamicReq { coverage :: [Position], -- the decisive positions
			       satisfied:: Bool,
			       logs     :: [String]
			     }
  deriving (Show)

newtype Requirement = Requirement {
  unRequirement :: Position ->        -- of this node
		   Int ->             -- index of the next child
		   Bool ->            -- only child?
		   (Int -> String) -> -- numerals to use in positions
		   StaticReq
  }

instance Show Requirement where
  show (Requirement r) = "Requirement ("++show (r (Position []) 1 True arabic)++")"

boolReq :: Bool -> Requirement
boolReq b = Requirement $ \(Position pos) child only numerals ->
  let pos' = Position $ if only then pos else pos++[numerals child]
      dyn  = DynamicReq { coverage = [pos'], satisfied = b, logs = []}
  in StaticReq {covered = [pos'], width = 1, dynamic = dyn}

logReq :: String -> Requirement
logReq s = Requirement $ \pos child only numerals ->
  StaticReq {covered = [], width = 0, dynamic = dyn}
  where dyn = DynamicReq { coverage = [], satisfied = True, logs = [s] }

notReq :: Requirement -> Requirement
notReq (Requirement r) = Requirement $ \pos child only numerals ->
  let sr = r pos child only numerals
      dr = dynamic sr
  in StaticReq { covered = covered sr,
     	       	 width   = width sr,
		 dynamic = DynamicReq {
		 	     satisfied = not (satisfied dr),
			     logs      = logs dr,
			     coverage  = coverage dr
			     }
	       }

instance Semigroup Requirement where
  Requirement r <> Requirement r' = Requirement $ \pos child only numerals ->
    let sr  = r pos child (only&&width sr'==0) numerals
        sr' = r' pos (child + width sr) (only&&width sr==0) numerals
	dr  = dynamic sr
	dr' = dynamic sr'
    in StaticReq {covered = covered sr++covered sr',
    	          width   = width sr  + width sr',
	          dynamic = DynamicReq {
		  	      satisfied = satisfied dr && satisfied dr',
			      logs      = logs dr      ++ logs dr',
			      coverage  = (if satisfied dr' then coverage dr  else []) ++
			      		  (if satisfied dr  then coverage dr' else [])
	                    }
		 }

instance Monoid Requirement where
  mempty = Requirement $ \pos child only numerals ->
    StaticReq {covered = [],
    	       width   = 0,
	       dynamic = DynamicReq {satisfied = True,
	       	       	 	     logs      = [],
				     coverage  = []}}

(#&&), (#||) :: Requirement -> Requirement -> Requirement

infixr 3 #&&
infixr 2 #||

(#&&) = (<>)

-- Disjunction. Note that disjunction of requirements uses lazy ||,
-- while #&& is strict. This is so that #|| can be used to define
-- implication (which needs to be lazy), while #&& correctly
-- identifies a failing requirement as decisive only if the other
-- operand does not fail--whichever operand it is. The other
-- strict/lazy variants can be obtained with double negation.

Requirement r #|| Requirement r' = Requirement $ \pos child only numerals ->
  let sr  = r pos child (only&&width sr'==0) numerals
      sr' = r' pos (child + width sr) (only&&width sr==0) numerals
      dr  = dynamic sr
      dr' = dynamic sr'  -- evaluated only if not (satisfied dr)
  in StaticReq {covered = covered sr++covered sr',
     	        width   = width sr  + width sr',
		dynamic = DynamicReq {
		            satisfied = satisfied dr || satisfied dr',
			    logs      = logs dr ++
			    	      	if satisfied dr then [] else logs dr',
			    coverage  = if satisfied dr then coverage dr
			    	        else if satisfied dr' then coverage dr'
					else coverage dr++coverage dr'
		            }
	       }

infixr 1 #=>

a #=> b = notReq a #|| b

-- Lift a boolean to a requirement without introducing a coverage
-- point. For use with #=> possibly, although using boolReq instead
-- would keep track of whether the precondition held or not. This
-- would require a test case that makes the precondition false.

liftBool b = Requirement $ \pos child only numerals ->
  StaticReq {covered = [],
  	     width   = 0,
	     dynamic = DynamicReq {
	       satisfied = b,
	       logs      = [],
	       coverage  = []
	     }
	    }

arabic, letter, capitals, roman :: Int -> String
arabic = show
letter = numeral ['a'..'z']
capitals = numeral ['A'..'Z']

numeral xs i = loop (i-1)
  where loop i | i < base = [xs!!i]
               | otherwise = loop (i `div` base)++[xs !! (i `mod` base)]
        base = length xs
  
roman n | 0 < n && n < 4000 = toRoman n
        | otherwise         = error $ "No roman numeral for "++show n
  where toRoman n | n < 10   = romanDigit 'i' 'v' 'x' n
	          | n < 100  = romanDigit 'x' 'l' 'c' (n `div` 10)  ++ toRoman (n `rem` 10)
		  | n < 1000 = romanDigit 'c' 'd' 'm' (n `div` 100) ++ toRoman (n `rem` 100)
		  | n < 4000 = romanDigit 'm'  u   u  (n `div` 1000)++ toRoman (n `rem` 1000)
          where u = error "Undefined roman numeral"
	romanDigit unit five ten n | n <  4 = replicate n unit
		   	     	   | n == 4 = [unit, five]
				   | n < 9  = five:replicate (n-5) unit
				   | n == 9 = [unit, ten]

groupReq numeralsG (Requirement r) = Requirement $ \(Position pos) child only numerals ->
  let sr = r pos' child' (not eliding || only) numerals'
      dr = dynamic sr
      eliding = width sr == 1  -- elide a group with one element
      child' = if eliding then child else 1
      pos' = Position $ if only then pos else pos ++ [numerals child]
      numerals' = if eliding then numerals else numeralsG
  in sr{width   = 1}

nameReq name req = Requirement $ \(Position pos) child only numerals ->
-- if req is a single requirement (width 1) then the line below leaves
-- req unchanged. Arabic numerals are used by default, but any
-- numerals can be used by grouping the argument explicitly.
  let Requirement r = groupReq arabic req
      pos' = if only then pos else pos ++ [numerals child]
  in r (Position (pos' ++ [name])) 1 True numerals
