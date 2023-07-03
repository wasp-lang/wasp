{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportName (..),
    importIdentifier,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import StrongPath (File', Path, Posix, Rel)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)

data ExtImport = ExtImport
  { -- | What is being imported.
    name :: ExtImportName,
    -- | Path from which we are importing.
    path :: ExtImportPath
  }
  deriving (Show, Eq, Data)

type ExtImportPath = Path Posix (Rel SourceExternalCodeDir) File'

type Identifier = String

data ExtImportName
  = -- | Represents external imports like @import Identifier from "file.js"@
    ExtImportModule Identifier
  | -- | Represents external imports like @import { Identifier } from "file.js"@
    ExtImportField Identifier
  deriving (Show, Eq, Data, Generic)

instance FromJSON ExtImportName

instance ToJSON ExtImportName

importIdentifier :: ExtImport -> Identifier
importIdentifier (ExtImport importName _) = case importName of
  ExtImportModule n -> n
  ExtImportField n -> n
