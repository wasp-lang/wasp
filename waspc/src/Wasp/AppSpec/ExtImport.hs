{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportName (..),
    importIdentifier,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
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
    kindStr <- o .: "kind"
    nameStr <- o .: "name"
    pathStr <- o .: "path"
    extImportName <- parseExtImportName kindStr nameStr
    extImportPath <- parseExtImportPath pathStr
    return $ ExtImport extImportName extImportPath
    where
      parseExtImportName kindStr nameStr = case kindStr of
        "default" -> pure $ ExtImportModule nameStr
        "named" -> pure $ ExtImportField nameStr
        _ -> fail $ "Failed to parse import kind: " <> kindStr

      parseExtImportPath pathStr = case parseRelFileP pathStr of
        Just path' -> pure path'
        Nothing -> fail $ "Failed to parse relative posix path to file: " <> pathStr

type ExtImportPath = Path Posix (Rel SourceExternalCodeDir) File'

type Identifier = String

data ExtImportName
  = -- | Represents external imports like @import Identifier from "file.js"@
    ExtImportModule Identifier
  | -- | Represents external imports like @import { Identifier } from "file.js"@
    ExtImportField Identifier
  deriving (Show, Eq, Data, Generic)

importIdentifier :: ExtImport -> Identifier
importIdentifier (ExtImport importName _) = case importName of
  ExtImportModule n -> n
  ExtImportField n -> n
