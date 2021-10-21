{-# LANGUAGE DeriveDataTypeable #-}

module AST.Core.Ref
  ( Ref (..),
  )
where

import Data.Data (Data)

-- TODO: Do we consider Ref to only ever point to declarations, or can it point to smth else?
--   If so, should we call it DeclRef? Or should 'name' be 'declName'?
--   Do we even care about concept of declaration here? Actually, that might be it -> if we don't castRel
--   about that as a concept at all, then we should not be thinking about it.

-- TODO: Is this `Data` deriving here really needed?
--   I saw that other types in tests are deriving this, but I am not sure if it is actually needed or not.
--   Docs of makeDeclType don't say that `Data` is needed. Maybe it is a left over?
--   I should probably try removing it, both from here and other places (tests).
--   Actually I think we can remove it! Should give it a try.
newtype Ref a = Ref {name :: String} deriving (Show, Eq, Data)
