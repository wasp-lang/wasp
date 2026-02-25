{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportName (..),
    ExtImportPath (..),
    importIdentifier,
    parseExtImportPath,
  )
where

import Control.Arrow (left)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
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
    -- | Path from which we are importing.
    path :: ExtImportPath
  }
  deriving (Show, Eq, Data)

instance FromJSON ExtImport where
  parseJSON = withObject "ExtImport" $ \o -> do
    kindStr <- o .: "kind"
    nameStr <- o .: "name"
    pathStr <- o .: "path"
    extImportName <- parseExtImportName kindStr nameStr
    extImportPath <- either fail pure $ parseExtImportPath pathStr
    return $ ExtImport extImportName extImportPath
    where
      parseExtImportName kindStr nameStr = case kindStr of
        "default" -> pure $ ExtImportModule nameStr
        "named" -> pure $ ExtImportField nameStr
        _ -> fail $ "Failed to parse import kind: " <> kindStr

data ExtImportPath
  = -- | Path relative to the user's source directory (from @src/ prefix).
    ExtImportSrcPath (Path Posix (Rel SourceExternalCodeDir) File')
  | -- | Verbatim package path (from @pkg/ prefix), e.g. "@acme/dashboard/src/queries.js".
    ExtImportPkgPath String
  deriving (Show, Eq, Data)

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

parseExtImportPath :: String -> Either String ExtImportPath
parseExtImportPath extImportPath
  | Just relFileFP <- stripPrefix extSrcPrefix extImportPath =
      left
        (("Failed to parse relative posix path to file: " ++) . show)
        (ExtImportSrcPath <$> SP.parseRelFileP relFileFP)
  | Just pkgPath <- stripPrefix extPkgPrefix extImportPath =
      Right $ ExtImportPkgPath pkgPath
  | otherwise =
      Left $
        "Path in external import must start with \""
          ++ extSrcPrefix
          ++ "\" or \""
          ++ extPkgPrefix
          ++ "\"!"
  where
    extSrcPrefix = "@src/"
    extPkgPrefix = "@pkg/"
