{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportName (..),
    importIdentifier,
    showExtImportFromProjectDir,
    showExtImportPathFromProjectDir,
  )
where

import Data.Aeson (FromJSON (parseJSON), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Data (Data)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import qualified StrongPath as SP
import qualified System.FilePath as FP
import Wasp.AppSpec.ExtImport.Source (ExtImportSource)
import qualified Wasp.AppSpec.ExtImport.Source as ExtImportSource
import qualified Wasp.Project.Common as Project

data ExtImport = ExtImport
  { -- | What is being imported.
    name :: ExtImportName,
    -- | Source from which we are importing.
    source :: ExtImportSource,
    -- | Local alias used in the Wasp config.
    alias :: Maybe Identifier
  }
  deriving (Show, Eq, Data)

instance ToJSON ExtImport where
  toJSON extImport =
    object
      [ "kind" .= kindStr,
        "name" .= nameStr,
        "source" .= extImportSourceToJSONFromProjectDir (source extImport),
        "alias" .= alias extImport
      ]
    where
      (kindStr, nameStr) = case name extImport of
        ExtImportModule n -> ("default" :: String, n)
        ExtImportField n -> ("named", n)

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

showExtImportFromProjectDir :: ExtImport -> String
showExtImportFromProjectDir extImport = importClause ++ " from \"" ++ showExtImportSourceFromProjectDir (source extImport) ++ "\""
  where
    importClause = case name extImport of
      ExtImportModule n -> withAlias n
      ExtImportField n -> "{ " ++ withAlias n ++ " }"
    withAlias n = case alias extImport of
      Just a | a /= n -> n ++ " as " ++ a
      _ -> n

showExtImportSourceFromProjectDir :: ExtImportSource -> String
showExtImportSourceFromProjectDir extImportSource = case extImportSource of
  ExtImportSource.ProjectSrcExtImportSource path -> showExtImportPathFromProjectDir path
  ExtImportSource.PackageExtImportSource packageImportSource ->
    ExtImportSource.packageImportSourceToImportSpecifier packageImportSource

showExtImportPathFromProjectDir :: ExtImportSource.ProjectSrcExtImportPath -> String
showExtImportPathFromProjectDir extImportPath
  | [".."] `isPrefixOf` FP.splitPath relPathStr = relPathStr
  | otherwise = FP.joinPath [".", relPathStr]
  where
    relPathStr = SP.fromRelFileP $ srcDirP SP.</> extImportPath

    srcDirP =
      fromMaybe
        (error "Internal error. Failed to convert srcDirInWaspProjectDir to POSIX. This should never happen.")
        (SP.relDirToPosix Project.srcDirInWaspProjectDir)

extImportSourceToJSONFromProjectDir :: ExtImportSource -> Value
extImportSourceToJSONFromProjectDir extImportSource = case extImportSource of
  ExtImportSource.ProjectSrcExtImportSource path ->
    object
      [ "kind" .= ("project-src" :: String),
        "path" .= showExtImportPathFromProjectDir path
      ]
  ExtImportSource.PackageExtImportSource packageImportSource ->
    object
      [ "kind" .= ("package" :: String),
        "packageName" .= ExtImportSource.packageName packageImportSource,
        "subpath" .= ExtImportSource.subpath packageImportSource
      ]
