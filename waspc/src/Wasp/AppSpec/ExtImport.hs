{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportSource (..),
    ExtImportName (..),
    PackageImportSource (..),
    ProjectSrcExtImportPath,
    importIdentifier,
    packageImportSourceToImportSpecifier,
    parseExtImportPath,
  )
where

import Control.Arrow (left)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (ToJSON)
import Data.Data (Data)
import Data.List (stripPrefix)
import GHC.Generics (Generic)
import StrongPath (File', Path, Posix, Rel)
import qualified StrongPath as SP
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)

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

type ProjectSrcExtImportPath = Path Posix (Rel SourceExternalCodeDir) File'

data ExtImportSource
  = ProjectSrcExtImportSource ProjectSrcExtImportPath
  | PackageExtImportSource PackageImportSource
  deriving (Show, Eq, Data)

instance FromJSON ExtImportSource where
  parseJSON = withObject "ExtImportSource" $ \o -> do
    kindStr <- o .: "kind"
    case kindStr of
      "project-src" -> do
        pathStr <- o .: "path"
        projectSrcPath <- either fail pure $ parseExtImportPath pathStr
        return $ ProjectSrcExtImportSource projectSrcPath
      "package" -> PackageExtImportSource <$> parseJSON (Aeson.Object o)
      _ -> fail $ "Failed to parse import source kind: " <> kindStr

data PackageImportSource = PackageImportSource
  { packageName :: String,
    subpath :: Maybe String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

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

packageImportSourceToImportSpecifier :: PackageImportSource -> String
packageImportSourceToImportSpecifier (PackageImportSource packageName maybeSubpath) =
  packageName ++ maybe "" ("/" ++) maybeSubpath

parseExtImportPath :: String -> Either String ProjectSrcExtImportPath
parseExtImportPath extImportPath = case stripImportPrefix extImportPath of
  Nothing -> Left $ "Path in external import must start with \"" ++ extSrcPrefix ++ "\"!"
  Just relFileFP ->
    left
      (("Failed to parse relative posix path to file: " ++) . show)
      $ SP.parseRelFileP relFileFP
  where
    stripImportPrefix importPath = stripPrefix extSrcPrefix importPath
    -- Filip: We no longer want separation between client and server code
    -- todo (filip): Do we still want to know which is which. We might (because of the reloading).
    -- For now, as we'd like (expect):
    --   - Nodemon watches all files in the user's source folder (client files
    --   included), but tsc only compiles the server files (I think because it
    --   knows that the others aren't used). I am not yet sure how it knows this.
    --   - Vite also only triggers on client files. I am not sure how it knows
    --   about the difference either.
    -- todo (filip): investigate
    extSrcPrefix = "@src/"
