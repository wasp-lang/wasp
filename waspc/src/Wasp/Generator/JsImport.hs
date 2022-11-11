module Wasp.Generator.JsImport
  ( getJsImportDetailsForExtFnImport,
  )
where

import StrongPath (Dir, Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)

getJsImportDetailsForExtFnImport ::
  -- | Path to generated external code directory, relative to the directory in which file doing the importing is.
  Path Posix (Rel a) (Dir GeneratedExternalCodeDir) ->
  AS.ExtImport.ExtImport ->
  -- | (importIdentifier, importStmt)
  --   - importIdentifier -> Identifier via which you can access ext js function after you import it with importStmt.
  --   - importStmt -> Javascript import statement via which you should do the import.
  (String, String)
getJsImportDetailsForExtFnImport relPosixPathToExtCodeDir extImport = (importIdentifier, importStmt)
  where
    importStmt = "import " ++ importWhat ++ " from '" ++ importFrom ++ "'"
    importFrom = "./" ++ SP.fromRelFileP (relPosixPathToExtCodeDir </> SP.castRel (AS.ExtImport.path extImport))
    (importIdentifier, importWhat) =
      case AS.ExtImport.name extImport of
        AS.ExtImport.ExtImportModule defaultImport -> (defaultImport, defaultImport)
        AS.ExtImport.ExtImportField namedImport -> (namedImport, "{ " ++ namedImport ++ " }")
