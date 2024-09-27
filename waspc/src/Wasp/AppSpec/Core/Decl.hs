{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Wasp.AppSpec.Core.Decl
  ( Decl,
    takeDecls,
    makeDecl,
    fromDecl,
  )
where

import Data.Maybe (mapMaybe)
import Data.Typeable (cast)
import Wasp.AppSpec.Core.IsDecl (IsDecl)

-- | A container for any (IsDecl a) type, allowing you to have a heterogenous list of
--   Wasp declarations as [Decl].
--   Declarations make the top level of AppSpec.
data Decl where
  Decl :: (IsDecl a) => String -> a -> Decl

-- | Extracts all declarations of a certain type from a @[Decl]@s
takeDecls :: (IsDecl a) => [Decl] -> [(String, a)]
takeDecls = mapMaybe fromDecl

makeDecl :: (IsDecl a) => String -> a -> Decl
makeDecl = Decl

fromDecl :: (IsDecl a) => Decl -> Maybe (String, a)
fromDecl (Decl name value) = (name,) <$> cast value
