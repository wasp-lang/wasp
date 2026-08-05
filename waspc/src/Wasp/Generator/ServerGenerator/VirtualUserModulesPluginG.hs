module Wasp.Generator.ServerGenerator.VirtualUserModulesPluginG
  ( genVirtualUserModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath
  ( relDirToPosix,
    relfile,
  )
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.VirtualUserModules as VUM
import Wasp.Generator.ServerGenerator.Common (serverSrcDirInServerRootDir)
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.Generator.ServerGenerator.JsImport as ServerJI

-- The plugin resolves server-side virtual user modules used by the SDK.
genVirtualUserModulesPlugin :: AppSpec -> Generator FileDraft
genVirtualUserModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      [relfile|src/plugins/virtualUserModules.js|]
      (Just $ object ["virtualUserModules" .= map mkPluginData (VUM.getServerVirtualUserModules spec)])
  where
    mkPluginData = VUM.mkVirtualUserModulePluginData extImportToImportJson
    extImportToImportJson = ServerJI.extImportToImportJson importLocation . Just
    importLocation = fromJust $ relDirToPosix serverSrcDirInServerRootDir
