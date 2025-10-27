{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Action
  ( Action (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity
import Wasp.AppSpec.ExtImport
import Wasp.AppSpec.JSON (maybeToField)

data Action = Action
  { fn :: ExtImport,
    entities :: Maybe [Ref Entity],
    auth :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance IsDecl Action

instance Aeson.ToJSON Action where
  toJSON action =
    let requiredFields = ["fn" Aeson..= fn action]
        optionalFields =
          [ maybeToField "entities" (entities action),
            maybeToField "auth" (auth action)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)
