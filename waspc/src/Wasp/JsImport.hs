{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.JsImport
  ( JsImport (..),
    JsImportName (..),
    JsImportData,
    JsImportAlias,
    getJsImportData,
  )
where

import Data.Data (Data)
import Data.List (isPrefixOf)
import StrongPath (Dir', File', Path, Posix, Rel)
import qualified StrongPath as SP

data JsImport = JsImport
  { -- | What is being imported.
    _name :: JsImportName,
    -- | Path from which we are importing.
    _path :: JsImportPath,
    -- | Alias for the imported name
    _importAlias :: Maybe JsImportAlias
  }
  deriving (Show, Eq, Data)

type JsImportPath = Path Posix (Rel Dir') File'

type Identifier = String

type JsImportAlias = String

data JsImportName
  = -- | Represents external imports like @import Identifier from "file.js"@
    JsImportModule Identifier
  | -- | Represents external imports like @import { Identifier } from "file.js"@
    JsImportField Identifier
  deriving (Show, Eq, Data)

type JsImportIdentifier = String

type JsImportWhat = String

type JsImportStatement = String

type JsImportData = (JsImportIdentifier, JsImportStatement)

getJsImportData :: JsImport -> JsImportData
getJsImportData (JsImport importName importPath importAlias) =
  (importIdentifier, importStatement)
  where
    (importIdentifier, importWhat) = getFirstPartOfJsImport importName importAlias
    importStatement = getImportStatement importWhat importPath

getImportStatement :: JsImportWhat -> Path Posix (Rel Dir') File' -> JsImportStatement
getImportStatement importWhat importPath =
  "import " ++ importWhat ++ " from '" ++ normalizedPath ++ "'"
  where
    filePath = SP.fromRelFileP importPath
    normalizedPath = if ".." `isPrefixOf` filePath then filePath else "./" ++ filePath

-- | Returns first part of import statement based on type of import and alias
--  e.g. for import { Name as Alias } from "file.js" it returns ("Alias", "{ Name as Alias }")
--  e.g. for import Name from "file.js" it returns ("Name", "Name")
getFirstPartOfJsImport :: JsImportName -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportWhat)
getFirstPartOfJsImport importName maybeImportAlias = case importName of
  JsImportModule defaultImport -> getForDefault defaultImport maybeImportAlias
  JsImportField namedImport -> getForNamed namedImport maybeImportAlias
  where
    getForDefault :: Identifier -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportWhat)
    getForDefault identifier Nothing = (identifier, identifier)
    getForDefault _ (Just importAlias) = (importAlias, importAlias)

    getForNamed :: Identifier -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportWhat)
    getForNamed identifier Nothing = (identifier, "{ " ++ identifier ++ " }")
    getForNamed identifier (Just importAlias) = (importAlias, "{ " ++ resolvedIdentifier ++ " }")
      where
        resolvedIdentifier =
          if identifier == importAlias then identifier else identifier ++ " as " ++ importAlias
