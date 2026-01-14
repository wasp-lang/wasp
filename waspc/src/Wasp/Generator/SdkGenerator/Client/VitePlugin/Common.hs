module Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( virtualFilesDirInVitePluginsDir,
    indexTsxVirtualFileName,
    routesTsxVirtualFileName,
  )
where

import StrongPath (Dir, Path', Rel, reldir)
import qualified Wasp.Generator.SdkGenerator.Common as C

data VirtualFilesDir

virtualFilesDirInVitePluginsDir :: Path' (Rel C.VitePluginsDir) (Dir VirtualFilesDir)
virtualFilesDirInVitePluginsDir = [reldir|virtual-files|]

indexTsxVirtualFileName :: String
indexTsxVirtualFileName = "index.virtual.tsx"

routesTsxVirtualFileName :: String
routesTsxVirtualFileName = "routes.virtual.tsx"
