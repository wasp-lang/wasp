{-# LANGUAGE DeriveDataTypeable #-}

module AppSpec.Page
  ( Page (..),
  )
where

import AppSpec.Core.Decl (IsDecl)
import Data.Data (Data)

data Page = Page
  { content :: String
  }
  deriving (Show, Eq, Data)

instance IsDecl Page
