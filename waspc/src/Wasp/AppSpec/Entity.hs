{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Entity (Entity (..), PSL (..)) where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)

data Entity = Entity PSL
  deriving (Show, Eq, Data)

instance IsDecl Entity

newtype PSL = PSL String deriving (Eq, Show, Data)
