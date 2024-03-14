{-# LANGUAGE TemplateHaskell #-}

module TemporalDemo where

-- Implement temporal logic by parameterising explicitly on a stream of events.

import Logical
import NameTree
import Numerals
import NamedReqs

now :: Requirement a -> Requirement [a]
now r = $(matching [| \xs -> anonymous $ boolean $ not (null xs) |]) #&&
        $(matching [| \(x:_) -> r `onValue` x |])


next :: Requirement [a] -> Requirement [a]
next r = $(matching [| \xs -> anonymous $ boolean $ not (null xs) |]) #&&
         $(matching [| \(_:xs) -> r `onValue` xs |])

end :: Requirement [a]
end = $(matching [| \xs -> anonymous $ boolean $ null xs |])

-------------

inRange =
  $(matching [| \n ->
      named "inRange" . group roman $
          (named "lowerBound" . boolean $ n >= 0)
      #&& (named "upperBound" . withError (show n ++" > 100") $ boolean $ n <= 100)
  |])

always r = recursively $ \rec -> end #|| (r #>&& next rec)

eventually r = recursively $ \rec -> r #>|| next rec

-- Hmm. Covering eventually r is tricky... it requires a stream in
-- which r holds ONLY ONCE. Not sure this is really what we want for
-- POSITIVE coverage.