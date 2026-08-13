module Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( virtualFilesDirInViteDir,
    virtualFilesFilesDirInViteDir,
    clientEntryPointPath,
    routesEntryPointPath,
    ssrEntryPointPath,
    spaFallbackFile,
    getPrerenderPaths,
  )
where

import StrongPath (Dir, File', Path, Path', Posix, Rel, reldir, relfileP, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Route as AS.Route
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

-- | The prerendered SPA shell. Static hosts (see our deployment providers)
-- serve it for any path they don't have a prerendered file for.
spaFallbackFile :: Path Posix (Rel WebAppViteBuildDir) File'
spaFallbackFile = [relfileP|200.html|]

-- | All the paths the user asked us to prerender at build time.
getPrerenderPaths :: AppSpec -> [String]
getPrerenderPaths spec = concatMap (AS.Route.prerender . snd) (AS.getRoutes spec)
