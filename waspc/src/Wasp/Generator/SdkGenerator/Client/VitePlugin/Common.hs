module Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( virtualFilesDirInViteDir,
    virtualFilesFilesDirInViteDir,
    clientEntryPointPath,
    routesEntryPointPath,
    ssrEntryPointPath,
    spaFallbackFile,
    userClientEnvSchemaVF,
    userSetupFnVF,
    userRootComponentVF,
    userPageVF,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Dir, Path', Rel, parseRelFileP, reldir, relfileP, (</>))
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
userClientEnvSchemaVF = [relfileP|virtual:wasp/user-client-env-schema|]

userSetupFnVF :: VirtualFile
userSetupFnVF = [relfileP|virtual:wasp/user-setup-fn|]

userRootComponentVF :: VirtualFile
userRootComponentVF = [relfileP|virtual:wasp/user-root-component|]

userPageVF :: String -> VirtualFile
userPageVF pageName = fromJust $ parseRelFileP $ "virtual:wasp/page/" ++ pageName
