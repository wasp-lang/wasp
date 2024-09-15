{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Wasp (Wasp (..)) where

import Data.Data (Data)
import Wasp.AppSpec.Core.IsDecl (IsDecl)

data Wasp = Wasp
  { version :: String
  }
  deriving (Show, Eq, Data)

instance IsDecl Wasp
