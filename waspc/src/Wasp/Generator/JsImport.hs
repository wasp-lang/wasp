module Wasp.Generator.JsImport
  ( getServerJsImport,
    getClientJsImport,
    getInternalJsImport,
    getImportStatementData,
    JsImportDefinition (..),
    JsImportScope (..),
  )
where

import Data.List
import Data.Maybe (fromJust)
import StrongPath (Dir', File', Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.ServerGenerator.ExternalCodeGenerator (extServerCodeDirInServerSrcDir)
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator (extClientCodeDirInWebAppSrcDir)

-- | Generates import statements from server ext-src.
getServerJsImport :: RelPathToRoot -> EI.ExtImport -> JsImportTemplateData
getServerJsImport relPathToRoot extImport =
  getImportStatementData (UserJsImportDefinition Server relPathToRoot extImport Nothing)

-- | Generates import statements from client ext-src.
getClientJsImport :: RelPathToRoot -> EI.ExtImport -> JsImportTemplateData
getClientJsImport relPathToRoot extImport =
  getImportStatementData (UserJsImportDefinition WebApp relPathToRoot extImport Nothing)

-- | Generates import statements based on custom path
getInternalJsImport :: EI.ExtImportName -> Path Posix (Rel Dir') File' -> JsImportTemplateData
getInternalJsImport importName importPath =
  getImportStatementData (InternaJslImportDefinition importName importPath Nothing)

-- Info which is needed to output import statements in generated code
data JsImportDefinition
  = -- | Internal imports, not based on user defined imports
    InternaJslImportDefinition
      { _importName :: EI.ExtImportName,
        _importPath :: Path Posix (Rel Dir') File',
        _internalImportAlias :: Maybe JsImportAlias
      }
  | -- | Imports based on user defined imports in .wasp file
    UserJsImportDefinition
      { _scope :: JsImportScope,
        _relPathToRoot :: RelPathToRoot,
        _extImport :: EI.ExtImport,
        _userImportAlias :: Maybe JsImportAlias
      }
  deriving (Show)

-- | User code is imported either on client or server side
data JsImportScope = WebApp | Server
  deriving (Show)

-- | Relative path to directory e.g. ../../ in ../../server/src/ext-src
type RelPathToRoot = Path Posix (Rel Dir') Dir'

-- | Alias in import e.g. @import { Identifier as Alias } from "file.js"@
type JsImportAlias = String

-- | JsImportIdentifier is used to access the imported module
--   JsImportStatement is the actual import statement
type JsImportTemplateData = (JsImportIdentifier, JsImportStatement)

-- | Generates import statements based on import definition
getImportStatementData :: JsImportDefinition -> JsImportTemplateData

-- | For internal imports
getImportStatementData (InternaJslImportDefinition importName importPath importAlias) =
  (importIdentifier, importStatement)
  where
    (importIdentifier, importWhat) = getFirstPartOfImport importName importAlias
    importStatement = getImportStatement importWhat importPath
-- For user defined imports
getImportStatementData (UserJsImportDefinition scope relPathToRoot extImport importAlias) =
  (importIdentifier, importStatement)
  where
    userDefinedPath = SP.castRel $ EI.path extImport
    webAppExtDir = fromJust (SP.relDirToPosix . SP.castRel $ extClientCodeDirInWebAppSrcDir)
    serverExtDir = fromJust (SP.relDirToPosix . SP.castRel $ extServerCodeDirInServerSrcDir)
    importFrom =
      case scope of
        WebApp -> relPathToRoot </> webAppExtDir </> userDefinedPath
        Server -> relPathToRoot </> serverExtDir </> userDefinedPath
    (importIdentifier, importWhat) = getFirstPartOfImport (EI.name extImport) importAlias
    importStatement = getImportStatement importWhat importFrom

type JsImportStatement = String

getImportStatement :: JsImportWhat -> Path Posix (Rel Dir') File' -> JsImportStatement
getImportStatement importWhat importPath =
  "import " ++ importWhat ++ " from '" ++ normalizedPath ++ "'"
  where
    filePath = SP.fromRelFileP importPath
    -- TODO: check if StrongPath can generate "./" prefixed paths
    normalizedPath = if ".." `isPrefixOf` filePath then filePath else "./" ++ filePath

type JsImportIdentifier = String

type JsImportWhat = String

-- | Returns first part of import statement based on type of import and alias
--  e.g. for import { Name as Alias } from "file.js" it returns ("Alias", "{ Name as Alias }")
--  e.g. for import Name from "file.js" it returns ("Name", "Name")
getFirstPartOfImport :: EI.ExtImportName -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportWhat)
getFirstPartOfImport extImportName importAlias = case extImportName of
  EI.ExtImportModule defaultImport -> getFirstPartOfDefaultImport defaultImport importAlias
  EI.ExtImportField namedImport -> getFirstPartOfNamedImport namedImport importAlias
  where
    getFirstPartOfDefaultImport :: String -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportWhat)
    getFirstPartOfDefaultImport name Nothing = (name, name)
    getFirstPartOfDefaultImport _ (Just importAlias') = (importAlias', importAlias')

    getFirstPartOfNamedImport :: String -> Maybe JsImportAlias -> (JsImportIdentifier, JsImportWhat)
    getFirstPartOfNamedImport name importAlias' = (name', "{ " ++ name' ++ " }")
      where
        name' = getName name importAlias'

        getName name'' Nothing = name''
        getName name'' (Just importAlias'')
          | name'' == importAlias'' = name''
          | otherwise = name'' ++ " as " ++ importAlias''
