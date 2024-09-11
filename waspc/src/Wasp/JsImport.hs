{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.JsImport
  ( JsImport (..),
    JsImportName (..),
    JsImportPath (..),
    JsImportAlias,
    JsImportIdentifier,
    JsImportStatement,
    makeJsImport,
    applyJsImportAlias,
    getImportIdentifier,
    getJsImportStmtAndIdentifier,
    getDynamicJsImportExprAndIdentifier,
  )
where

import Data.Data (Data)
import Data.List (isPrefixOf)
import StrongPath (Dir', File', Path, Posix, Rel)
import qualified StrongPath as SP

-- | Represents a JS import data type that can be used to generate import statements
--   in generated code. It doesn't fully support all types of JS imports (multiple imports)
--   but this is enough for our current use case.
data JsImport = JsImport
  { -- | Path from which we are importing.
    _path :: JsImportPath,
    -- | What is being imported. NOTE: We don't currenly support multiple names in one statement,
    --   that's why it's "name" and not "names".
    _name :: JsImportName,
    -- | Alias for the imported name.
    _importAlias :: Maybe JsImportAlias
  }
  deriving (Show, Eq, Data)

data JsImportPath
  = RelativeImportPath (Path Posix (Rel Dir') File')
  | ModuleImportPath (Path Posix (Rel Dir') File')
  deriving (Show, Eq, Data)

-- Note (filip): not a fan of so many aliases for regular types
type JsImportAlias = String

data JsImportName
  = -- | Represents external imports like @import Identifier from "file.js"@
    JsImportModule JsImportIdentifier
  | -- | Represents external imports like @import { Identifier } from "file.js"@
    JsImportField JsImportIdentifier
  deriving (Show, Eq, Data)

type JsImportIdentifier = String

-- | Represents the left side of the import statement, between @import@ and @from@,
--   e.g. @{ Name }@ or @{ Name as Alias }@ or @NameForDefault@ .
type JsImportClause = String

-- | Represents the full import statement e.g. @import { Name } from "file.js"@
type JsImportStatement = String

-- | Represents the object destructuring part of a dynamic import statement e.g. @const { Name } = await import("file.js")@
type JsImportObject = String

getImportIdentifier :: JsImport -> JsImportIdentifier
getImportIdentifier JsImport {_name = name} = case name of
  JsImportModule identifier -> identifier
  JsImportField identifier -> identifier

makeJsImport :: JsImportPath -> JsImportName -> JsImport
makeJsImport importPath importName = JsImport importPath importName Nothing

applyJsImportAlias :: Maybe JsImportAlias -> JsImport -> JsImport
applyJsImportAlias importAlias jsImport = jsImport {_importAlias = importAlias}

getJsImportStmtAndIdentifier :: JsImport -> (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier (JsImport importPath importName maybeImportAlias) =
  getJsImportStmtAndIdentifierRaw (getImportFilePath importPath) importName maybeImportAlias

getImportFilePath :: JsImportPath -> FilePath
getImportFilePath (RelativeImportPath relPath) = normalizePath $ SP.fromRelFileP relPath
  where
    normalizePath path = if ".." `isPrefixOf` path then path else "./" ++ path
getImportFilePath (ModuleImportPath pathString) = SP.fromRelFileP pathString

-- todo(filip): attempt to simplify how we generate imports. I wanted to generate a
-- module import (e.g., '@ext-src/something') and couldn't do it. This is one of
-- the funtions I implemented while I was trying to pull it off.
getJsImportStmtAndIdentifierRaw ::
  FilePath ->
  JsImportName ->
  Maybe JsImportAlias ->
  (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifierRaw importPath importName maybeImportAlias =
  (importStatement, importIdentifier)
  where
    (importIdentifier, importClause) = jsImportIdentifierAndClause
    importStatement = "import " ++ importClause ++ " from '" ++ importPath ++ "'"

    -- First part of import statement based on type of import and alias
    -- e.g. for import { Name as Alias } from "file.js" it returns ("Alias", "{ Name as Alias }")
    -- e.g. for import Name from "file.js" it returns ("Name", "Name")
    jsImportIdentifierAndClause :: (JsImportIdentifier, JsImportClause)
    jsImportIdentifierAndClause = case importName of
      JsImportModule defaultImport -> getForDefault defaultImport maybeImportAlias
      JsImportField namedImport -> getForNamed namedImport maybeImportAlias
      where
        getForDefault :: JsImportIdentifier -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportClause)
        getForDefault identifier Nothing = (identifier, identifier)
        getForDefault _ (Just importAlias) = (importAlias, importAlias)

        getForNamed :: JsImportIdentifier -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportClause)
        getForNamed identifier Nothing = (identifier, "{ " ++ identifier ++ " }")
        getForNamed identifier (Just importAlias) = (importAlias, "{ " ++ resolvedIdentifier ++ " }")
          where
            resolvedIdentifier =
              if identifier == importAlias then identifier else identifier ++ " as " ++ importAlias

getDynamicJsImportExprAndIdentifier :: JsImport -> (JsImportStatement, JsImportIdentifier)
getDynamicJsImportExprAndIdentifier (JsImport importPath importName maybeImportAlias) =
  getDynamicJsImportExprAndIdentifierRaw (getImportFilePath importPath) importName maybeImportAlias

getDynamicJsImportExprAndIdentifierRaw ::
  FilePath ->
  JsImportName ->
  Maybe JsImportAlias ->
  (JsImportStatement, JsImportIdentifier)
getDynamicJsImportExprAndIdentifierRaw importPath importName maybeImportAlias =
  (importExpr, importIdentifier)
  where
    (importIdentifier, importObject) = jsImportIdentifierAndObject
    importExpr = "const " ++ importObject ++ " = await import('" ++ importPath ++ "')"

    -- First part of a dynamic import statement e.g. @{ default: Name }@
    -- in @const { default: Name } = await import("file.js")@
    jsImportIdentifierAndObject :: (JsImportIdentifier, JsImportObject)
    jsImportIdentifierAndObject = case importName of
      JsImportModule defaultImport -> getForDefault defaultImport maybeImportAlias
      JsImportField namedImport -> getForNamed namedImport maybeImportAlias
      where
        getForDefault :: JsImportIdentifier -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportObject)
        getForDefault identifier Nothing = (identifier, "{ default: " ++ identifier ++ " }")
        getForDefault _ (Just importAlias) = (importAlias, "{ default: " ++ importAlias ++ " }")

        getForNamed :: JsImportIdentifier -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportObject)
        getForNamed identifier Nothing = (identifier, "{ " ++ identifier ++ " }")
        getForNamed identifier (Just importAlias) = (importAlias, "{ " ++ resolvedIdentifier ++ " }")
          where
            resolvedIdentifier =
              if identifier == importAlias then identifier else identifier ++ ": " ++ importAlias
