{-# LANGUAGE TemplateHaskell #-}

module ReqLib where

import NamedReqs

allReq r = recursively $ \restReq ->
  $(matching [| \(x:xs) -> (r `onValue` x) #&& (restReq `onValue` xs) |])
