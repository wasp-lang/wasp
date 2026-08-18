module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualUserModulesPluginG
  ( genVirtualUserModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile, (</>))
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.SdkGenerator.VirtualUserModules as VUM

-- The plugin resolves client-side virtual user modules used by the SDK.
genVirtualUserModulesPlugin :: AppSpec -> Generator FileDraft
genVirtualUserModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      (C.vitePluginsDirInSdkTemplatesDir </> [relfile|virtualUserModules.ts|])
      (object ["virtualUserModules" .= map mkPluginData (VUM.getClientVirtualUserModules spec)])
  where
    mkPluginData = VUM.mkVirtualUserModulePluginData extImportToImportJson
    extImportToImportJson =
      GJI.jsImportToImportJson . Just . GJI.extImportToRelativeSrcImportFromViteExecution
