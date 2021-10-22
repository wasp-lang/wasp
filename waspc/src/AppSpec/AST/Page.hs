{-# LANGUAGE DeriveDataTypeable #-}

module AppSpec.AST.Page
  ( Page (..),
  )
where

import Data.Data (Data)

data Page = Page
  { content :: String
  }
  deriving (Show, Eq, Data)
