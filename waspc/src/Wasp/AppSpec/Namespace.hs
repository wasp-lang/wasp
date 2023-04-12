{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Namespace
  ( Namespace (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)

data Namespace = Namespace
  { middlewareConfigFn :: ExtImport,
    path :: String
  }
  deriving (Show, Eq, Data)

instance IsDecl Namespace
