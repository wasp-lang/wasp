{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Entity (Entity (..), PSL (..)) where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)

-- | TODO: Entity should be much more complex, it should not be just String, instead it should be
-- whole AST that describes the entity schema.
-- We should take a look at what entity looks like Wasp.Wasp.Entity and replicate that.
-- Most challenging part will be parsing PSL from string into PSL.Ast.Model and then
-- building Entity based on that. We actually have all that logic already, we just need to plug it in
-- in the right places and that should be it.
data Entity = Entity PSL
  deriving (Show, Eq, Data)

instance IsDecl Entity

newtype PSL = PSL String deriving (Eq, Show, Data)
