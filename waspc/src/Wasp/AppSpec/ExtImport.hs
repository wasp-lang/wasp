{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportName (..),
    importIdentifier,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Aeson.Types (ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.ExtImport.Source (ExtImportSource)

data ExtImport = ExtImport
  { -- | What is being imported.
    name :: ExtImportName,
    -- | Source from which we are importing.
    source :: ExtImportSource,
    -- | Local alias used in the Wasp config.
    alias :: Maybe Identifier
  }
  deriving (Show, Eq, Data)

instance FromJSON ExtImport where
  parseJSON = withObject "ExtImport" $ \o -> do
    kindStr <- o .: "kind"
    nameStr <- o .: "name"
    source <- o .: "source"
    aliasStr <- o .:? "alias"
    extImportName <- parseExtImportName kindStr nameStr
    return $ ExtImport extImportName source aliasStr
    where
      parseExtImportName kindStr nameStr = case kindStr of
        "default" -> pure $ ExtImportModule nameStr
        "named" -> pure $ ExtImportField nameStr
        _ -> fail $ "Failed to parse import kind: " <> kindStr

type Identifier = String

data ExtImportName
  = -- | Represents external imports like @import Identifier from "file.js"@
    ExtImportModule Identifier
  | -- | Represents external imports like @import { Identifier } from "file.js"@
    ExtImportField Identifier
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

importIdentifier :: ExtImport -> Identifier
importIdentifier (ExtImport importName _ maybeAlias) = case maybeAlias of
  Just aliasName -> aliasName
  Nothing -> case importName of
    ExtImportModule n -> n
    ExtImportField n -> n
