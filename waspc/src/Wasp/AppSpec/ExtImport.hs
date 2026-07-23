{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportName (..),
    importIdentifier,
    parseExtImportPath,
    showExtImport,
    showExtImportPath,
  )
where

import Control.Arrow (left)
import Data.Aeson (FromJSON (parseJSON), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Data (Data)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import StrongPath (File', Path, Posix, Rel)
import qualified StrongPath as SP
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)
import qualified Wasp.Project.Common as Project

data ExtImport = ExtImport
  { -- | What is being imported.
    name :: ExtImportName,
    -- | Path from which we are importing.
    path :: ExtImportPath,
    -- | Local alias used in the Wasp config.
    alias :: Maybe Identifier
  }
  deriving (Show, Eq, Data)

instance ToJSON ExtImport where
  toJSON extImport =
    object
      [ "kind" .= kindStr,
        "name" .= nameStr,
        "path" .= showExtImportPath (path extImport),
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
    pathStr <- o .: "path"
    aliasStr <- o .:? "alias"
    extImportName <- parseExtImportName kindStr nameStr
    extImportPath <- either fail pure $ parseExtImportPath pathStr
    return $ ExtImport extImportName extImportPath aliasStr
    where
      parseExtImportName kindStr nameStr = case kindStr of
        "default" -> pure $ ExtImportModule nameStr
        "named" -> pure $ ExtImportField nameStr
        _ -> fail $ "Failed to parse import kind: " <> kindStr

type ExtImportPath = Path Posix (Rel SourceExternalCodeDir) File'

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

parseExtImportPath :: String -> Either String ExtImportPath
parseExtImportPath extImportPath = case stripImportPrefix extImportPath of
  Nothing -> Left $ "Path in external import must start with \"" ++ extSrcPrefix ++ "\"!"
  Just relFileFP ->
    left
      (("Failed to parse relative posix path to file: " ++) . show)
      $ SP.parseRelFileP relFileFP
  where
    stripImportPrefix = stripPrefix extSrcPrefix

showExtImportPath :: ExtImportPath -> String
showExtImportPath extImportPath = SP.fromRelFileP (srcDirAsPosix SP.</> extImportPath)
  where
    srcDirAsPosix = fromMaybe (error "Internal error. Failed to convert srcDirInWaspProjectDir to POSIX. This should never happen.") $ SP.relDirToPosix Project.srcDirInWaspProjectDir

-- | Renders an external import the way the user would write it, e.g.
-- @{ tasks } from "@src/queries.ts"@ or @Main from "@src/pages/Main.tsx"@.
showExtImport :: ExtImport -> String
showExtImport extImport = importClause ++ " from \"" ++ showExtImportPath (path extImport) ++ "\""
  where
    importClause = case name extImport of
      ExtImportModule n -> withAlias n
      ExtImportField n -> "{ " ++ withAlias n ++ " }"
    withAlias n = case alias extImport of
      Just a | a /= n -> n ++ " as " ++ a
      _ -> n

-- Filip: We no longer want separation between client and server code
-- todo (filip): Do we still want to know which is which. We might (because of the reloading).
-- For now, as we'd like (expect):
--   - Nodemon watches all files in the user's source folder (client files
--   included), but tsc only compiles the server files (I think because it
--   knows that the others aren't used). I am not yet sure how it knows this.
--   - Vite also only triggers on client files. I am not sure how it knows
--   about the difference either.
-- todo (filip): investigate
extSrcPrefix :: String
extSrcPrefix = "@src/"
