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

-- The plugin resolves the virtual user modules used by the SDK.
--
-- It resolves the server-side ones too, not just the client-side ones: Vite
-- builds and serves the app's server as well, so both end up in bundles Vite
-- makes. Listing a module here doesn't put it in any bundle, it only says where
-- to find it if something imports it.
genVirtualUserModulesPlugin :: AppSpec -> Generator FileDraft
genVirtualUserModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      (C.vitePluginsDirInSdkTemplatesDir </> [relfile|virtualUserModules.ts|])
      (object ["virtualUserModules" .= virtualUserModules])
  where
    virtualUserModules =
      map mkPluginData . VUM.uniqueByVirtualModuleId $ VUM.getVirtualUserModules spec

    mkPluginData = VUM.mkVirtualUserModulePluginData extImportToImportJson
    extImportToImportJson =
      GJI.jsImportToImportJson . Just . GJI.extImportToRelativeSrcImportFromViteExecution
