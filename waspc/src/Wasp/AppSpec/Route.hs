{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Route
  ( Route (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Page

-- | NOTE: This is new syntax for route, before it was `route "/task" -> page Task`.
-- Also, routes now need to have name -> how do we feel about that? Are we ok with that?
data Route = Route
  { path :: String,
    page :: Ref Page
  }
  deriving (Show, Eq, Data)

instance IsDecl Route
