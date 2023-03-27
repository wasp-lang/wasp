{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Route
  ( Route (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Page

data Route = Route
  { path :: String,
    -- TODO: In the future we might want to add other types of targets, for example another Route.
    --   For that the best solution is probably to implement sum types (https://github.com/wasp-lang/wasp/issues/381).
    to :: Ref Page
  }
  deriving (Show, Eq, Data)

instance IsDecl Route