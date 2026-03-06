module Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( virtualFilesDirInViteDir,
    virtualFilesFilesDirInViteDir,
    clientEntryPointPath,
    routesEntryPointPath,
    userClientEnvSchemaPath,
    userClientEnvSchemaVF,
    userSetupFnVF,
    userRootComponentVF,
    pageVF,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Dir, Path', Rel, reldir, relfileP, (</>))
import qualified StrongPath as SP
import Wasp.JsImport (VirtualFile)
import qualified Wasp.Generator.SdkGenerator.Common as C

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

userClientEnvSchemaPath :: String
userClientEnvSchemaPath = "virtual:wasp/user-client-env"

userClientEnvSchemaVF :: VirtualFile
userClientEnvSchemaVF = [relfileP|virtual:wasp/user-client-env|]

userSetupFnVF :: VirtualFile
userSetupFnVF = [relfileP|virtual:wasp/user-setup-fn|]

userRootComponentVF :: VirtualFile
userRootComponentVF = [relfileP|virtual:wasp/user-root-component|]

pageVF :: String -> VirtualFile
pageVF pageName = fromJust $ SP.parseRelFileP $ "virtual:wasp/page/" ++ pageName
