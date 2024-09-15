{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ApiNamespace
  ( ApiNamespace (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)

data ApiNamespace = ApiNamespace
  { middlewareConfigFn :: ExtImport,
    path :: String
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

instance IsDecl ApiNamespace
