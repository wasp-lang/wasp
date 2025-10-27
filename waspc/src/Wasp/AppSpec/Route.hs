{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Route
  ( Route (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Page

data Route = Route
  { path :: String,
    -- TODO: In the future we might want to add other types of targets, for example another Route.
    --   For that the best solution is probably to implement sum types (https://github.com/wasp-lang/wasp/issues/381).
    to :: Ref Page
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Route
