module Wasp.Generator.SdkGenerator.Client.VitePlugin.WaspVirtualUserModulesPluginG
  ( genWaspVirtualUserModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (maybeToList)
import StrongPath (File', Path, Posix, Rel, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getVirtualUserModuleJsImportPath)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.JsImport (getJsImportPathStringFromPath)

-- The plugin resolves client-side virtual user modules used by the SDK.
genWaspVirtualUserModulesPlugin :: AppSpec -> Generator FileDraft
genWaspVirtualUserModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      (C.vitePluginsDirInSdkTemplatesDir </> [relfile|waspVirtualUserModules.ts|])
      (object ["virtualUserModules" .= getClientVirtualUserModulesData spec])

getClientVirtualUserModulesData :: AppSpec -> [Aeson.Value]
getClientVirtualUserModulesData spec =
  maybeToList (mkVMImportData <$> maybeClientEnvSchema)
  where
    mkVMImportData :: EI.ExtImport -> Aeson.Value
    mkVMImportData extImport =
      object
        [ "virtualModuleId" .= virtualModuleId,
          "importJson" .= importJson
        ]
      where
        importJson = GJI.jsImportToImportJson (Just jsImport)
        jsImport = GJI.extImportToRelativeSrcImportFromViteExecution extImport

        virtualModuleId = getJsImportPathStringFromPath $ getVirtualUserModuleJsImportPath userDefinedPathInExtSrcDir
        userDefinedPathInExtSrcDir = SP.castRel $ EI.path extImport :: Path Posix (Rel SourceExternalCodeDir) File'

    maybeClientEnvSchema = AS.App.client app >>= AS.App.Client.envValidationSchema
    app = snd $ getApp spec
