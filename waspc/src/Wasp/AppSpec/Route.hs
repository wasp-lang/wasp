{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Route
  ( Route (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Page

-- | NOTE: We have new syntax for route, before it was `route "/task" -> page Task`, now it is a dictionary.
data Route = Route
  { path :: String,
    page :: Ref Page
  }
  deriving (Show, Eq, Data)

instance IsDecl Route
