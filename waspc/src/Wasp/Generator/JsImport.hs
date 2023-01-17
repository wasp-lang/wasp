module Wasp.Generator.JsImport
  ( genJsImport,
    mkImportStatement,
    mkImportStatementWithAlias,
    mkImportStatementFromRelPath,
  )
where

import StrongPath (Dir, Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)

data ImportStatement = ImportStatement
  { _extImportName :: AS.ExtImport.ExtImportName,
    _path :: FilePath,
    _importAlias :: Maybe String
  }
  deriving (Show)

-- | Generates JS import statement based on: ExtImportName, path and optional import alias
genJsImport ::
  ImportStatement ->
  -- | (importIdentifier, importStmt)
  --   - importIdentifier -> Identifier via which you can access ext js function after you import it with importStmt.
  --   - importStmt -> Javascript import statement via which you should do the import.
  (String, String)
genJsImport (ImportStatement extImportName path importAlias) = (importIdentifier, "import " ++ importExpr ++ " from '" ++ path ++ "'")
  where
    (importIdentifier, importExpr) = getImportExpr extImportName importAlias

getImportExpr :: AS.ExtImport.ExtImportName -> Maybe String -> (String, String)
getImportExpr extImportName importAlias = (importIdentifier, importExpr)
  where
    (importIdentifier, importExpr) =
      case extImportName of
        AS.ExtImport.ExtImportModule defaultImport -> getDefaultImportExpr defaultImport importAlias
        AS.ExtImport.ExtImportField namedImport -> getNamedImportExpr namedImport importAlias

-- | If there is an import alias defined, use that as the import name
-- If the alias is "SignupPage" then instead of "Signup" the import
-- statement should be @import SignupPage from "..."@
getDefaultImportExpr :: String -> Maybe String -> (String, String)
getDefaultImportExpr name Nothing = (name, name)
getDefaultImportExpr _ (Just importAlias) = (importAlias, importAlias)

-- | For named imports, we need to render @import { Name } from "..."@
getNamedImportExpr :: String -> Maybe String -> (String, String)
getNamedImportExpr name importAlias = (identifier, "{ " ++ identifier ++ " }")
  where
    identifier = getNamedImportIdentifier name importAlias

-- | If an alias is defined, the names import becomes @import { Name as Alias } from "..."@
getNamedImportIdentifier :: String -> Maybe String -> String
getNamedImportIdentifier identifier Nothing = identifier
getNamedImportIdentifier identifier (Just importAlias)
  | identifier == importAlias = identifier
  | otherwise = identifier ++ " as " ++ importAlias

mkImportStatement :: AS.ExtImport.ExtImportName -> FilePath -> ImportStatement
mkImportStatement extImportName path =
  ImportStatement {_extImportName = extImportName, _path = path, _importAlias = Nothing}

mkImportStatementWithAlias :: AS.ExtImport.ExtImportName -> FilePath -> String -> ImportStatement
mkImportStatementWithAlias extImportName path importAlias =
  ImportStatement {_extImportName = extImportName, _path = path, _importAlias = Just importAlias}

mkImportStatementFromRelPath ::
  -- | Path to generated external code directory, relative to the directory in which file doing the importing is.
  Path Posix (Rel a) (Dir GeneratedExternalCodeDir) ->
  AS.ExtImport.ExtImport ->
  ImportStatement
mkImportStatementFromRelPath relPosixPathToExtCodeDir extImport = importStatement
  where
    importFrom = "./" ++ SP.fromRelFileP (relPosixPathToExtCodeDir </> SP.castRel (AS.ExtImport.path extImport))
    importStatement = mkImportStatement (AS.ExtImport.name extImport) importFrom
