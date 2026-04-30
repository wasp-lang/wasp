{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.JsImport
  ( JsImport (..),
    JsImportKind (..),
    JsImportName (..),
    JsImportPath (..),
    JsImportAlias,
    JsImportIdentifier,
    JsImportStatement,
    makeJsImport,
    applyJsImportAlias,
    getImportIdentifier,
    getJsImportStmtAndIdentifier,
    getJsImportPathString,
    getJsDynamicImportExpression,
  )
where

import Data.Data (Data)
import Data.List (isPrefixOf)
import StrongPath (Dir', File', Path, Posix, Rel)
import qualified StrongPath as SP

-- | Represents a JS import data type that can be used to generate import statements
--   in generated app. It doesn't fully support all types of JS imports (multiple imports)
--   but this is enough for our current use case.
data JsImport = JsImport
  { -- | Whether the import is a type-only import or a value (runtime) import.
    _kind :: JsImportKind,
    -- | Path from which we are importing.
    _path :: JsImportPath,
    -- | What is being imported. NOTE: We don't currenly support multiple names in one statement,
    --   that's why it's "name" and not "names".
    _name :: JsImportName,
    -- | Alias for the imported name.
    _importAlias :: Maybe JsImportAlias
  }
  deriving (Show, Eq, Data)

-- | Distinguishes type-only imports (used in @typeof@ positions and as @import type@)
--   from value imports that bring runtime bindings into scope.
data JsImportKind
  = TypeImport
  | ValueImport
  deriving (Show, Eq, Data)

data JsImportPath
  = RelativeImportPath (Path Posix (Rel Dir') File')
  | ModuleImportPath (Path Posix (Rel Dir') File')
  | RawImportName String
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

getImportIdentifier :: JsImport -> JsImportIdentifier
getImportIdentifier JsImport {_name = name} = case name of
  JsImportModule identifier -> identifier
  JsImportField identifier -> identifier

makeJsImport :: JsImportKind -> JsImportPath -> JsImportName -> JsImport
makeJsImport importKind importPath importName =
  JsImport
    { _kind = importKind,
      _path = importPath,
      _name = importName,
      _importAlias = Nothing
    }

applyJsImportAlias :: Maybe JsImportAlias -> JsImport -> JsImport
applyJsImportAlias importAlias jsImport = jsImport {_importAlias = importAlias}

getJsImportStmtAndIdentifier :: JsImport -> (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier jsImport =
  (importStatement, importIdentifier)
  where
    importStatement = importKeyword ++ " " ++ importClause ++ " from '" ++ getJsImportPathString jsImport ++ "'"
    importKeyword = case jsImport._kind of
      TypeImport -> "import type"
      ValueImport -> "import"
    (importIdentifier, importClause) = getJsImportIdentifierAndClause jsImport._name jsImport._importAlias

getJsImportPathString :: JsImport -> String
getJsImportPathString jsImport = case jsImport._path of
  RelativeImportPath relPath -> normalizePath $ SP.fromRelFileP relPath
  ModuleImportPath modulePath -> SP.fromRelFileP modulePath
  RawImportName moduleName -> moduleName
  where
    normalizePath path
      | ".." `isPrefixOf` path = path
      | otherwise = "./" ++ path

-- | Returns a dynamic import expression. The shape depends on the import kind:
--   * named type export: @import('./path').Name@
--   * named value export: @import('./path').then(m => m.Name)@
getJsDynamicImportExpression :: JsImport -> String
getJsDynamicImportExpression jsImport = case jsImport._kind of
  TypeImport -> "import('" ++ importPath ++ "')." ++ memberName
  ValueImport -> "import('" ++ importPath ++ "').then(m => m." ++ memberName ++ ")"
  where
    importPath = getJsImportPathString jsImport
    memberName = case jsImport._name of
      JsImportModule _ -> "default"
      JsImportField name -> name

-- First part of import statement based on type of import and alias
-- e.g. for import { Name as Alias } from "file.js" it returns ("Alias", "{ Name as Alias }")
-- e.g. for import Name from "file.js" it returns ("Name", "Name")
getJsImportIdentifierAndClause :: JsImportName -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportClause)
getJsImportIdentifierAndClause importName maybeImportAlias = case (importName, maybeImportAlias) of
  (JsImportModule moduleName, Nothing) -> (moduleName, moduleName)
  (JsImportModule _, Just alias) -> (alias, alias)
  (JsImportField fieldName, Nothing) -> (fieldName, "{ " ++ fieldName ++ " }")
  (JsImportField fieldName, Just alias)
    | fieldName == alias -> (fieldName, "{ " ++ fieldName ++ " }")
    | otherwise -> (alias, "{ " ++ fieldName ++ " as " ++ alias ++ " }")
