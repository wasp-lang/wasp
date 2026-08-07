module Wasp.Generator.TypeAugmentationGenerator.App.Sdk.JsImport
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
import Wasp.Generator.TypeAugmentationGenerator.App.Sdk.Common (SdkTypeAugmentationRootDir, sdkTypeAugmentationRootDirInGeneratedCodeDir)
import Wasp.JsImport (JsImport (..), JsImportKind (TypeImport), JsImportPath (..))
import Wasp.Project.Common (srcDirInWaspProjectDir, waspProjectDirFromGeneratedAppDir)
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
extImportToJsImport extImport@(EI.ExtImport extImportName extImportSource _) =
  JsImport
    { _kind = TypeImport,
      _path = importPath,
      _name = GJI.extImportNameToJsImportName extImportName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
  where
    importPath = GJI.extImportSourceToJsImportPath projectSrcPathToJsImportPath extImportSource
    projectSrcPathToJsImportPath projectSrcPath =
      RelativeImportPath $ dropExtensionFromImportPath $ SP.castRel $ extSrcDirFromSdkTypesRootDir </> projectSrcPath

extSrcDirFromSdkTypesRootDir :: Path Posix (Rel SdkTypeAugmentationRootDir) (Dir SourceExternalCodeDir)
extSrcDirFromSdkTypesRootDir =
  SP.castRel $
    fromJust $
      relDirToPosix $
        generatedCodeDirFromTypesRootDir </> waspProjectDirFromGeneratedAppDir </> srcDirInWaspProjectDir
  where
    generatedCodeDirFromTypesRootDir = invertRelDir sdkTypeAugmentationRootDirInGeneratedCodeDir
