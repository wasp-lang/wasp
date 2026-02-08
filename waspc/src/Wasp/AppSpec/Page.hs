{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Page
  ( Page (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)

data Page = Page
  { component :: ExtImport,
    authRequired :: Maybe Bool,
    ssr :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

instance IsDecl Page
