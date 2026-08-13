module Wasp.Generator.SdkGenerator.Server.VitePlugin.VirtualUserModulesPluginG
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

-- The plugin resolves server-side virtual user modules used by the SDK.
genVirtualUserModulesPlugin :: AppSpec -> Generator FileDraft
genVirtualUserModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      (C.serverVitePluginsDirInSdkTemplatesDir </> [relfile|virtualUserModules.ts|])
      (object ["virtualUserModules" .= VUM.mkVirtualUserModulesPluginData extImportToImportJson (VUM.getServerVirtualUserModules spec)])
  where
    extImportToImportJson =
      GJI.jsImportToImportJson . Just . GJI.extImportToRelativeSrcImportFromViteExecution
