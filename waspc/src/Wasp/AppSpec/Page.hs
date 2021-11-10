{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Page
  ( Page (..),
  )
where

import Wasp.AppSpec.Core.Decl (IsDecl)
import Data.Data (Data)

data Page = Page
  { content :: String
  }
  deriving (Show, Eq, Data)

instance IsDecl Page
