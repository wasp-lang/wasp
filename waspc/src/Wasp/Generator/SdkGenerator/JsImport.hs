module Wasp.Generator.SdkGenerator.JsImport
  ( extOperationImportToImportJson,
    extImportToImportJson,
    extImportToJsImport,
  )
where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (castRel, relDirToPosix, (</>))
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.Common (dropExtensionFromImportPath)
import Wasp.Generator.JsImport (getAliasedExtImportIdentifier)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.SdkGenerator.Common
  ( extSrcDirInSdkRootDir,
    makeSdkImportPath,
  )
import Wasp.JsImport (JsImport (..), JsImportPath (..))

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
extImportToJsImport extImport =
  case EI.path extImport of
    EI.ExtImportSrcPath srcPath ->
      JsImport
        { _path = ModuleImportPath importPath,
          _name = importName,
          _importAlias = Just $ getAliasedExtImportIdentifier extImport
        }
      where
        importPath = makeSdkImportPath $ dropExtensionFromImportPath $ extCodeDirP </> castRel srcPath
        extCodeDirP = fromJust $ relDirToPosix extSrcDirInSdkRootDir
    EI.ExtImportPkgPath pkgPath ->
      JsImport
        { _path = RawModuleImportPath pkgPath,
          _name = importName,
          _importAlias = Just $ getAliasedExtImportIdentifier extImport
        }
  where
    importName = GJI.extImportNameToJsImportName $ EI.name extImport
