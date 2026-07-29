module Wasp.Generator.SdkGenerator.Client.VitePlugin.WaspVirtualUserModulesPluginG
  ( genWaspVirtualUserModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile, (</>))
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.SdkGenerator.VirtualUserModules
  ( getClientVirtualUserModules,
    mkVirtualUserModulePluginData,
  )

-- The plugin resolves client-side virtual user modules used by the SDK.
genWaspVirtualUserModulesPlugin :: AppSpec -> Generator FileDraft
genWaspVirtualUserModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      (C.vitePluginsDirInSdkTemplatesDir </> [relfile|waspVirtualUserModules.ts|])
      (object ["virtualUserModules" .= map mkPluginData (getClientVirtualUserModules spec)])
  where
    mkPluginData = mkVirtualUserModulePluginData extImportToImportJson
    extImportToImportJson =
      GJI.jsImportToImportJson . Just . GJI.extImportToRelativeSrcImportFromViteExecution
