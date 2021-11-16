{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Page
  ( Page (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)

data Page = Page
  { content :: String
  }
  deriving (Show, Eq, Data)

instance IsDecl Page
