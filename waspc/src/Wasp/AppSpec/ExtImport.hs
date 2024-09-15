{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportName (..),
    importIdentifier,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
import Data.Data (Data)
import GHC.Generics (Generic)
import StrongPath (File', Path, Posix, Rel, parseRelFileP)
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)

data ExtImport = ExtImport
  { -- | What is being imported.
    name :: ExtImportName,
    -- | Path from which we are importing.
    path :: ExtImportPath
  }
  deriving (Show, Eq, Data, Generic)

instance FromJSON ExtImport where
  parseJSON = withObject "ExtImport" $ \o -> do
    name' <- o .: "name"
    pathStr <- o .: "path"
    case parseRelFileP pathStr of
      Just path' -> pure $ ExtImport {name = name', path = path'}
      Nothing -> fail $ "Failed to parse relative posix path to file: " <> pathStr

type ExtImportPath = Path Posix (Rel SourceExternalCodeDir) File'

type Identifier = String

data ExtImportName
  = -- | Represents external imports like @import Identifier from "file.js"@
    ExtImportModule Identifier
  | -- | Represents external imports like @import { Identifier } from "file.js"@
    ExtImportField Identifier
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

importIdentifier :: ExtImport -> Identifier
importIdentifier (ExtImport importName _) = case importName of
  ExtImportModule n -> n
  ExtImportField n -> n
