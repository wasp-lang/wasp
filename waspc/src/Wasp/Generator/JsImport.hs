module Wasp.Generator.JsImport
  ( getImportDetailsForJsFnImport,
  )
where

import StrongPath (Dir, Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import qualified Wasp.Wasp.JsImport as Wasp.JsImport

getImportDetailsForJsFnImport ::
  -- | Path to generated external code directory, relative to the directory in which file doing the importing is.
  Path Posix (Rel (Dir a)) (Dir GeneratedExternalCodeDir) ->
  Wasp.JsImport.JsImport ->
  -- | (importIdentifier, importStmt)
  --   - importIdentifier -> Identifier via which you can access js function after you import it with importStmt.
  --   - importStmt -> Import statement via which you should do the import.
  (String, String)
getImportDetailsForJsFnImport relPosixPathToExtCodeDir jsImport = (importIdentifier, importStmt)
  where
    importStmt = "import " ++ importWhat ++ " from '" ++ importFrom ++ "'"
    importFrom = "./" ++ SP.fromRelFileP (relPosixPathToExtCodeDir </> SP.castRel (Wasp.JsImport._from jsImport))
    (importIdentifier, importWhat) =
      case (Wasp.JsImport._defaultImport jsImport, Wasp.JsImport._namedImports jsImport) of
        (Just defaultImport, []) -> (defaultImport, defaultImport)
        (Nothing, [namedImport]) -> (namedImport, "{ " ++ namedImport ++ " }")
        _ -> error $ "Expected from " ++ show jsImport ++ " to be either default import or single named import, due to it being used to import a single js function."
