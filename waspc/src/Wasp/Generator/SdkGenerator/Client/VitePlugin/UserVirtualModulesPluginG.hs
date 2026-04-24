module Wasp.Generator.SdkGenerator.Client.VitePlugin.UserVirtualModulesPluginG
  ( genUserVirtualModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (maybeToList)
import StrongPath (relfile, toFilePath, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.UserVirtualModules
  ( VirtualFile,
    userClientEnvSchemaVF,
    userClientRootComponentVF,
    userClientSetupFnVF,
  )

genUserVirtualModulesPlugin :: AppSpec -> Generator FileDraft
genUserVirtualModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      (C.vitePluginsDirInSdkTemplatesDir </> [relfile|userVirtualModules.ts|])
      (object ["userVirtualModules" .= getClientUserVirtualModulesData spec])

getClientUserVirtualModulesData :: AppSpec -> [Aeson.Value]
getClientUserVirtualModulesData spec =
  maybeToList (mkImportData userClientEnvSchemaVF <$> maybeClientEnvSchema)
    ++ maybeToList (mkImportData userClientSetupFnVF <$> maybeSetupFn)
    ++ maybeToList (mkImportData userClientRootComponentVF <$> maybeRootComponent)
  where
    mkImportData :: VirtualFile -> EI.ExtImport -> Aeson.Value
    mkImportData vf extImport =
      object
        [ "virtualPath" .= toFilePath vf,
          "importJson" .= importJson
        ]
      where
        importJson = GJI.jsImportToImportJson (Just jsImport)
        jsImport = GJI.extImportToRelativeSrcImportFromViteExecution extImport

    maybeClientEnvSchema = AS.App.client app >>= AS.App.Client.envValidationSchema
    maybeSetupFn = AS.App.Client.setupFn =<< AS.App.client app
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client app
    app = snd $ getApp spec
