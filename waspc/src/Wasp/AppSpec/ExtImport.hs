{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ExtImport
  ( ExtImport (..),
    ExtImportName (..),
    importIdentifier,
    parseExtImportPath,
  )
where

import Control.Arrow (left)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Aeson.Types (ToJSON)
import Data.Data (Data)
import Data.List (stripPrefix)
import GHC.Generics (Generic)
import StrongPath (File', Path, Posix, Rel, toFilePath)
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

instance ToJSON ExtImport where
  toJSON (ExtImport importName importPath) =
    let kindStr = case importName of
          ExtImportModule _ -> "default" :: String
          ExtImportField _ -> "named" :: String
        nameStr = importIdentifier (ExtImport importName importPath)
        pathStr = toFilePath importPath
     in Aeson.object
          [ "kind" Aeson..= kindStr,
            "name" Aeson..= nameStr,
            "path" Aeson..= pathStr
          ]

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

parseExtImportPath :: String -> Either String ExtImportPath
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
