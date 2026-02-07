{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPluginG
  ( getVirtualModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( clientEntryPointPath,
    routesEntryPointPath,
    virtualFilesDirInSdkTemplatesUserCoreDir,
    virtualFilesFilesDirInSdkTemplatesUserCoreDir,
    vitePluginsDirInSdkTemplatesUserCoreDir,
  )
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPlugin.VirtualRoutesG (genVirtualRoutesTsx)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( genFileCopy,
    mkTmplFdWithData,
  )

getVirtualModulesPlugin :: AppSpec -> Generator [FileDraft]
getVirtualModulesPlugin spec =
  sequence
    [ getVirtualModulesTs,
      genVirtualFilesResolverTs,
      genVirtualFilesIndexTs,
      genVirtualIndexTsx spec,
      genVirtualRoutesTsx spec
    ]

genVirtualFilesIndexTs :: Generator FileDraft
genVirtualFilesIndexTs =
  genFileCopy $ virtualFilesDirInSdkTemplatesUserCoreDir </> [relfile|index.ts|]

genVirtualFilesResolverTs :: Generator FileDraft
genVirtualFilesResolverTs =
  genFileCopy $ virtualFilesDirInSdkTemplatesUserCoreDir </> [relfile|resolver.ts|]

getVirtualModulesTs :: Generator FileDraft
getVirtualModulesTs =
  return $
    mkTmplFdWithData
      (vitePluginsDirInSdkTemplatesUserCoreDir </> [relfile|virtualModules.ts|])
      tmplData
  where
    tmplData =
      object
        [ "clientEntryPointPath" .= clientEntryPointPath,
          "routesEntryPointPath" .= routesEntryPointPath
        ]

genVirtualIndexTsx :: AppSpec -> Generator FileDraft
genVirtualIndexTsx spec =
  return $
    mkTmplFdWithData
      (virtualFilesFilesDirInSdkTemplatesUserCoreDir </> [relfile|index.tsx|])
      tmplData
  where
    tmplData =
      object
        [ "setupFn" .= GJI.jsImportToImportJson (GJI.extImportToRelativeSrcImportFromViteExecution <$> maybeSetupJsFunction),
          "rootComponent" .= GJI.jsImportToImportJson (GJI.extImportToRelativeSrcImportFromViteExecution <$> maybeRootComponent),
          "routesMapping" .= routesMappingImportJson
        ]
    maybeSetupJsFunction = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)
    routesMappingImportJson =
      object
        [ "isDefined" .= True,
          "importStatement" .= ("import { routesMapping } from \"" ++ routesEntryPointPath ++ "\""),
          "importIdentifier" .= ("routesMapping" :: String)
        ]
