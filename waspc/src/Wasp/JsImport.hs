{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.JsImport
  ( JsImport (..),
    JsImportName (..),
    JsImportAlias,
    JsImportPath,
    JsImportIdentifier,
    JsImportStatement,
    getJsImportIdentiiferAndStmtFromAnyPath,
    getJsImportStmtAndIdentifier,
  )
where

import Data.Data (Data)
import Data.List (isPrefixOf)
import StrongPath (Dir', File', Path, Posix, Rel)
import qualified StrongPath as SP

-- | Represents a JS import data type that can be used to generate import statements
--   in generated code. It doesn't fully support all JS import types (like multiple imports)
--   but this is enough for our current use case.
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

type JsImportAlias = String

data JsImportName
  = -- | Represents external imports like @import Identifier from "file.js"@
    JsImportModule JsImportIdentifier
  | -- | Represents external imports like @import { Identifier } from "file.js"@
    JsImportField JsImportIdentifier
  deriving (Show, Eq, Data)

type JsImportIdentifier = String

-- | Represents the left side of the import statement e.g. { Name } or { Name as Alias } or NameForDefault
type JsImportWhat = String

type JsImportStatement = String

-- | Generates JS import statement based on any path
--   and without using an import alias since that's the
--   most common use case.
getJsImportIdentiiferAndStmtFromAnyPath :: JsImportPath -> JsImportName -> (JsImportStatement, JsImportIdentifier)
getJsImportIdentiiferAndStmtFromAnyPath importPath importName = getJsImportStmtAndIdentifier importPath importName Nothing

getJsImportStmtAndIdentifier :: JsImportPath -> JsImportName -> Maybe JsImportAlias -> (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier importPath importName maybeImportAlias =
  (importStatement, importIdentifier)
  where
    (importIdentifier, importWhat) = jsImportIdentifierAndWhat

    importStatement :: JsImportStatement
    importStatement = "import " ++ importWhat ++ " from '" ++ normalizedPath ++ "'"
      where
        filePath = SP.fromRelFileP importPath
        normalizedPath = if ".." `isPrefixOf` filePath then filePath else "./" ++ filePath

    -- First part of import statement based on type of import and alias
    -- e.g. for import { Name as Alias } from "file.js" it returns ("Alias", "{ Name as Alias }")
    -- e.g. for import Name from "file.js" it returns ("Name", "Name")
    jsImportIdentifierAndWhat :: (JsImportIdentifier, JsImportWhat)
    jsImportIdentifierAndWhat = case importName of
      JsImportModule defaultImport -> getForDefault defaultImport maybeImportAlias
      JsImportField namedImport -> getForNamed namedImport maybeImportAlias
      where
        getForDefault :: JsImportIdentifier -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportWhat)
        getForDefault identifier Nothing = (identifier, identifier)
        getForDefault _ (Just importAlias) = (importAlias, importAlias)

        getForNamed :: JsImportIdentifier -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportWhat)
        getForNamed identifier Nothing = (identifier, "{ " ++ identifier ++ " }")
        getForNamed identifier (Just importAlias) = (importAlias, "{ " ++ resolvedIdentifier ++ " }")
          where
            resolvedIdentifier =
              if identifier == importAlias then identifier else identifier ++ " as " ++ importAlias
