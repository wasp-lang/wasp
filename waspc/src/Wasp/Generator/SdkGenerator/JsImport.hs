module Wasp.Generator.SdkGenerator.JsImport
  ( extOperationImportToImportJson,
    extImportToImportJson,
    extImportToJsImport,
    extImportToSdkSrcRelativeImport,
  )
where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath ((</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.Common (dropExtensionFromImportPath)
import qualified Wasp.Generator.JsImport as GJI
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.JsImport (JsImport (..), JsImportPath (..))
import Wasp.Project.Common (srcDirInWaspProjectDir)

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
    { _path = ModuleImportPath importPath,
      _name = importName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
  where
    importPath = C.makeSdkImportPath $ dropExtensionFromImportPath $ extCodeDirP </> SP.castRel extImportPath
    extCodeDirP = fromJust $ SP.relDirToPosix C.extSrcDirInSdkRootDir
    importName = GJI.extImportNameToJsImportName extImportName

extImportToSdkSrcRelativeImport :: EI.ExtImport -> JsImport
extImportToSdkSrcRelativeImport extImport@(EI.ExtImport extImportName extImportPath) =
  JsImport
    { _path = RelativeImportPath relativePath,
      _name = importName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
  where
    relativePath = SP.castRel $ dropExtensionFromImportPath $ projectSrcDir </> extImportPath
    projectSrcDir = fromJust (SP.relDirToPosix srcDirInWaspProjectDir)
    importName = GJI.extImportNameToJsImportName extImportName

getAliasedExtImportIdentifier :: EI.ExtImport -> String
getAliasedExtImportIdentifier extImport = EI.importIdentifier extImport ++ "_ext"
