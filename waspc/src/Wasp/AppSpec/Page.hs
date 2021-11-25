{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Page
  ( Page (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)

data Page = Page
  { component :: ExtImport,
    authRequired :: Maybe Bool
  }
  deriving (Show, Eq, Data)

instance IsDecl Page
