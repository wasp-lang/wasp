{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Db
  ( Db (..),
    DbSystem (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (maybeToField)

data Db = Db
  { seeds :: Maybe [ExtImport],
    prismaSetupFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON Db where
  toJSON db =
    let optionalFields =
          [ maybeToField "seeds" (seeds db),
            maybeToField "prismaSetupFn" (prismaSetupFn db)
          ]
     in Aeson.object (catMaybes optionalFields)

data DbSystem = PostgreSQL | SQLite
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON, Aeson.ToJSON)
