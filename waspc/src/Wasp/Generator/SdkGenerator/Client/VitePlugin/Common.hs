module Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( virtualFilesDirInViteDir,
    virtualFilesFilesDirInViteDir,
    clientEntryPointPath,
    routesEntryPointPath,
    ssrEntryPointPath,
    spaFallbackFile,
    userClientEnvSchemaVF,
    userClientSetupFnVF,
    userClientRootComponentVF,
  )
where

import StrongPath (Dir, Path', Rel, reldir, relfileP, (</>))
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.JsImport (VirtualFile)

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

spaFallbackFile :: String
spaFallbackFile = "200.html"

userClientEnvSchemaVF :: VirtualFile
userClientEnvSchemaVF = [relfileP|virtual:wasp/user/client-env-schema|]

userClientSetupFnVF :: VirtualFile
userClientSetupFnVF = [relfileP|virtual:wasp/user/client-setup-fn|]

userClientRootComponentVF :: VirtualFile
userClientRootComponentVF = [relfileP|virtual:wasp/user/client-root-component|]
