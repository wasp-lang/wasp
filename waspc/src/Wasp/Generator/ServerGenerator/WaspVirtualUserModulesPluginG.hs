module Wasp.Generator.ServerGenerator.WaspVirtualUserModulesPluginG
  ( genWaspVirtualUserModulesPlugin,
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
import Wasp.Generator.SdkGenerator.VirtualUserModules
  ( getServerVirtualUserModules,
    mkVirtualUserModulePluginData,
  )
import Wasp.Generator.ServerGenerator.Common (serverSrcDirInServerRootDir)
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.Generator.ServerGenerator.JsImport as ServerJI

-- The plugin resolves server-side virtual user modules used by the SDK.
genWaspVirtualUserModulesPlugin :: AppSpec -> Generator FileDraft
genWaspVirtualUserModulesPlugin spec =
  return $
    C.mkTmplFdWithData
      [relfile|src/plugins/waspVirtualUserModules.js|]
      (Just $ object ["virtualUserModules" .= map mkPluginData (getServerVirtualUserModules spec)])
  where
    mkPluginData = mkVirtualUserModulePluginData extImportToImportJson
    extImportToImportJson = ServerJI.extImportToImportJson importLocation . Just
    importLocation = fromJust $ relDirToPosix serverSrcDirInServerRootDir
