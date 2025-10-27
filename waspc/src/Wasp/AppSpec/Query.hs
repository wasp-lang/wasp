{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Query
  ( Query (..),
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

data Query = Query
  { fn :: ExtImport,
    entities :: Maybe [Ref Entity],
    auth :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance IsDecl Query

instance Aeson.ToJSON Query where
  toJSON query =
    let requiredFields = ["fn" Aeson..= fn query]
        optionalFields =
          [ maybeToField "entities" (entities query),
            maybeToField "auth" (auth query)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)
