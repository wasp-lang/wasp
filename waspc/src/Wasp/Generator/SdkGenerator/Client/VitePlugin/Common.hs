module Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( virtualFilesDirInViteDir,
    virtualFilesFilesDirInViteDir,
    clientEntryPointPath,
    ssrEntryPointPath,
    routesEntryPointPath,
  )
where

import StrongPath (Dir, Path', Rel, reldir, (</>))
import qualified Wasp.Generator.SdkGenerator.Common as C

data VirtualFilesDir

data VirtualFilesFilesDir

virtualFilesDirInViteDir :: Path' (Rel C.ViteDir) (Dir VirtualFilesDir)
virtualFilesDirInViteDir = [reldir|virtual-files|]

virtualFilesFilesDirInViteDir :: Path' (Rel C.ViteDir) (Dir VirtualFilesFilesDir)
virtualFilesFilesDirInViteDir = virtualFilesDirInViteDir </> [reldir|files|]

clientEntryPointPath :: String
clientEntryPointPath = "/@wasp/client-entry.tsx"

ssrEntryPointPath :: String
ssrEntryPointPath = "/@wasp/entry-server.tsx"

routesEntryPointPath :: String
routesEntryPointPath = "/@wasp/routes.tsx"
