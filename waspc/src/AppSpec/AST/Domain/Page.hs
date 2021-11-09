{-# LANGUAGE DeriveDataTypeable #-}

module AppSpec.AST.Domain.Page
  ( Page (..),
  )
where

import Data.Data (Data)

data Page = Page
  { content :: String
  }
  deriving (Show, Eq, Data)
