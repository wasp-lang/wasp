module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToJsImport,
    extOperationImportToImportJson,
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
import qualified Wasp.JsImport as JI

extImportToJsImport :: EI.ExtImport -> JsImport
extImportToJsImport extImport@(EI.ExtImport extImportName extImportPath) =
  JsImport
    { _path = ModuleImportPath importPath,
      _name = importName,
      _importAlias = Just $ EI.importIdentifier extImport ++ "_ext"
    }
  where
    importPath = C.makeSdkImportPath $ dropExtensionFromImportPath $ extCodeDirP </> SP.castRel extImportPath
    extCodeDirP = fromJust $ SP.relDirToPosix C.extSrcDirInSdkRootDir
    importName = GJI.extImportNameToJsImportName extImportName

extOperationImportToImportJson :: EI.ExtImport -> Aeson.Value
extOperationImportToImportJson =
  GJI.jsImportToImportJson
    . Just
    . applyExtImportAlias
    . extImportToJsImport
  where
    applyExtImportAlias jsImport =
      jsImport {_importAlias = Just $ JI.getImportIdentifier jsImport ++ "_ext"}
