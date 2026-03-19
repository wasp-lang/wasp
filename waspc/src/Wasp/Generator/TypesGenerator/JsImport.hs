module Wasp.Generator.TypesGenerator.JsImport
  ( extOperationImportToImportJson,
    extImportToImportJson,
  )
where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (Dir, Path, Posix, Rel, relDirToPosix, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)
import Wasp.Generator.Common (dropExtensionFromImportPath)
import Wasp.Generator.JsImport (getAliasedExtImportIdentifier)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.TypesGenerator.Common (TypesRootDir, typesRootDirInGeneratedCodeDir)
import Wasp.JsImport (JsImport (..), JsImportPath (..))
import Wasp.Project.Common (srcDirInWaspProjectDir, waspProjectDirFromGeneratedCodeDir)
import Wasp.Util.StrongPath (invertRelDir)

extImportToImportJson :: Maybe EI.ExtImport -> Aeson.Value
extImportToImportJson maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport <$> maybeExtImport

extOperationImportToImportJson :: EI.ExtImport -> Aeson.Value
extOperationImportToImportJson =
  GJI.jsImportToImportJson
    . Just
    . extImportToJsImport

extImportToJsImport :: EI.ExtImport -> JsImport
extImportToJsImport extImport@(EI.ExtImport extImportName extImportPath) =
  JsImport
    { _path = RelativeImportPath importPath,
      _name = GJI.extImportNameToJsImportName extImportName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
  where
    importPath = dropExtensionFromImportPath $ SP.castRel $ extSrcDirFromTypesRootDir </> extImportPath

extSrcDirFromTypesRootDir :: Path Posix (Rel TypesRootDir) (Dir SourceExternalCodeDir)
extSrcDirFromTypesRootDir =
  SP.castRel $
    fromJust $
      relDirToPosix $
        generatedCodeDirFromTypesRootDir </> waspProjectDirFromGeneratedCodeDir </> srcDirInWaspProjectDir
  where
    generatedCodeDirFromTypesRootDir = invertRelDir typesRootDirInGeneratedCodeDir
