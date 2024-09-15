{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Action
  ( Action (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity
import Wasp.AppSpec.ExtImport

data Action = Action
  { fn :: ExtImport,
    entities :: Maybe [Ref Entity],
    auth :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

instance IsDecl Action
