module Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( virtualFilesDirInViteDir,
    virtualFilesFilesDirInViteDir,
    clientEntryPointPath,
    routesEntryPointPath,
    ssrEntryPointPath,
    spaFallbackFile,
  )
where

import StrongPath (Dir, File', Path, Path', Posix, Rel, reldir, relfileP, (</>))
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.WebAppGenerator (WebAppViteBuildDir)

data VirtualFilesDir

data VirtualFilesFilesDir

virtualFilesDirInViteDir :: Path' (Rel C.ViteDir) (Dir VirtualFilesDir)
virtualFilesDirInViteDir = [reldir|virtual-files|]

virtualFilesFilesDirInViteDir :: Path' (Rel C.ViteDir) (Dir VirtualFilesFilesDir)
virtualFilesFilesDirInViteDir = virtualFilesDirInViteDir </> [reldir|files|]

clientEntryPointPath :: String
clientEntryPointPath = "/@wasp/client-entry.tsx"

routesEntryPointPath :: String
routesEntryPointPath = "/@wasp/routes.tsx"

ssrEntryPointPath :: String
ssrEntryPointPath = "/@wasp/ssr-entry.tsx"

spaFallbackFile :: Path Posix (Rel WebAppViteBuildDir) File'
spaFallbackFile = [relfileP|200.html|]
