module Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( virtualFilesDirInViteDir,
    virtualFilesFilesDirInViteDir,
    VirtualModuleId,
    clientEntryPointVMId,
    routesEntryPointVMId,
    ssrEntryPointVMId,
    spaFallbackFile,
  )
where

import StrongPath (Dir, Dir', File', Path, Path', Posix, Rel, reldir, relfileP, (</>))
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.WebAppGenerator (WebAppViteBuildDir)

data VirtualFilesDir

data VirtualFilesFilesDir

virtualFilesDirInViteDir :: Path' (Rel C.ViteDir) (Dir VirtualFilesDir)
virtualFilesDirInViteDir = [reldir|virtual-files|]

virtualFilesFilesDirInViteDir :: Path' (Rel C.ViteDir) (Dir VirtualFilesFilesDir)
virtualFilesFilesDirInViteDir = virtualFilesDirInViteDir </> [reldir|files|]

type VirtualModuleId = Path Posix (Rel Dir') File'

clientEntryPointVMId :: VirtualModuleId
clientEntryPointVMId = [relfileP|virtual:wasp/client-entry.tsx|]

routesEntryPointVMId :: VirtualModuleId
routesEntryPointVMId = [relfileP|virtual:wasp/routes.tsx|]

ssrEntryPointVMId :: VirtualModuleId
ssrEntryPointVMId = [relfileP|virtual:wasp/ssr-entry.tsx|]

spaFallbackFile :: Path Posix (Rel WebAppViteBuildDir) File'
spaFallbackFile = [relfileP|200.html|]
