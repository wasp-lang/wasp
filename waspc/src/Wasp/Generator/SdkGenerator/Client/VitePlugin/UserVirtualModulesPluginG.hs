module Wasp.Generator.SdkGenerator.Client.VitePlugin.UserVirtualModulesPluginG
  ( genUserVirtualModulesPlugin,
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

genUserVirtualModulesPlugin :: AppSpec -> Generator FileDraft
genUserVirtualModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      (C.vitePluginsDirInSdkTemplatesDir </> [relfile|userVirtualModules.ts|])
      (object ["userVirtualModules" .= getClientUserVirtualModulesData spec])

getClientUserVirtualModulesData :: AppSpec -> [Aeson.Value]
getClientUserVirtualModulesData spec =
  maybeToList (mkVMImportData <$> maybeClientEnvSchema)
    ++ maybeToList (mkVMImportData <$> maybeSetupFn)
    ++ maybeToList (mkVMImportData <$> maybeRootComponent)
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
    maybeSetupFn = AS.App.Client.setupFn =<< AS.App.client app
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client app
    app = snd $ getApp spec
