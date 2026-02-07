module Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( viteDirInSdkTemplatesUserCoreDir,
    vitePluginsDirInSdkTemplatesUserCoreDir,
    virtualFilesDirInSdkTemplatesUserCoreDir,
    virtualFilesFilesDirInSdkTemplatesUserCoreDir,
    clientEntryPointPath,
    routesEntryPointPath,
  )
where

import StrongPath (Dir, Path', Rel, reldir, (</>))
import Wasp.Generator.SdkGenerator.UserCore.Common (SdkTemplatesUserCoreDir)

data ViteDir

data VitePluginsDir

data VirtualFilesDir

data VirtualFilesFilesDir

viteDirInSdkTemplatesUserCoreDir :: Path' (Rel SdkTemplatesUserCoreDir) (Dir ViteDir)
viteDirInSdkTemplatesUserCoreDir = [reldir|client/vite|]

vitePluginsDirInSdkTemplatesUserCoreDir :: Path' (Rel SdkTemplatesUserCoreDir) (Dir VitePluginsDir)
vitePluginsDirInSdkTemplatesUserCoreDir = viteDirInSdkTemplatesUserCoreDir </> [reldir|plugins|]

virtualFilesDirInSdkTemplatesUserCoreDir :: Path' (Rel SdkTemplatesUserCoreDir) (Dir VirtualFilesDir)
virtualFilesDirInSdkTemplatesUserCoreDir = viteDirInSdkTemplatesUserCoreDir </> [reldir|virtual-files|]

virtualFilesFilesDirInSdkTemplatesUserCoreDir :: Path' (Rel SdkTemplatesUserCoreDir) (Dir VirtualFilesFilesDir)
virtualFilesFilesDirInSdkTemplatesUserCoreDir = virtualFilesDirInSdkTemplatesUserCoreDir </> [reldir|files|]

clientEntryPointPath :: String
clientEntryPointPath = "/@wasp/client-entry.tsx"

routesEntryPointPath :: String
routesEntryPointPath = "/@wasp/routes.tsx"
