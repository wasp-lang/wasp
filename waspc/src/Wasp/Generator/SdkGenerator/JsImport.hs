module Wasp.Generator.SdkGenerator.JsImport
  ( extOperationImportToImportJson,
    extImportToImportJson,
  )
where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath ((</>), castRel, relDirToPosix)
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.Common (dropExtensionFromImportPath)
import qualified Wasp.Generator.JsImport as GJI
import qualified Wasp.Generator.SdkGenerator.Common as C
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
extImportToJsImport extImport@(EI.ExtImport extImportName extImportPath) =
  JsImport
    { _path = ModuleImportPath importPath,
      _name = importName,
      _importAlias = Just $ EI.importIdentifier extImport ++ "_ext"
    }
  where
    importPath = C.makeSdkImportPath $ dropExtensionFromImportPath $ extCodeDirP </> castRel extImportPath
    extCodeDirP = fromJust $ relDirToPosix C.extSrcDirInSdkRootDir
    importName = GJI.extImportNameToJsImportName extImportName
