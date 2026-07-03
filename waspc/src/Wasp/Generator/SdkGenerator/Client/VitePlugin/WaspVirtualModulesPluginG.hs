{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.WaspVirtualModulesPluginG
  ( getWaspVirtualModulesPlugin,
  )
where

import Data.Aeson (Value, object, (.=))
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import StrongPath.Path (toPathRelFileP)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (clientEntryPointVMId, routesEntryPointVMId, spaFallbackFile, ssrEntryPointVMId, virtualFilesDirInViteDir, virtualFilesFilesDirInViteDir)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.WaspVirtualModulesPlugin.VirtualRoutesG (genVirtualRoutesTsx)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.JsImport (JsImportName (JsImportField), JsImportPath (ModuleImportPath), makeValueJsImport)

getWaspVirtualModulesPlugin :: AppSpec -> Generator [FileDraft]
getWaspVirtualModulesPlugin spec =
  sequence
    [ getWaspVirtualModulesTs,
      genVirtualFilesResolverTs,
      genVirtualFilesIndexTs,
      genVirtualClientEntryTsx spec,
      genVirtualSsrEntryTsx spec,
      genVirtualRoutesTsx spec
    ]

genVirtualFilesIndexTs :: Generator FileDraft
genVirtualFilesIndexTs =
  return $
    C.mkTmplFd tmplPath
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesDirInViteDir </> [relfile|index.ts|]

genVirtualFilesResolverTs :: Generator FileDraft
genVirtualFilesResolverTs =
  return $
    C.mkTmplFd tmplPath
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesDirInViteDir </> [relfile|resolver.ts|]

getWaspVirtualModulesTs :: Generator FileDraft
getWaspVirtualModulesTs =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|waspVirtualModules.ts|]
    tmplData =
      object
        [ "clientEntryPointVMId" .= toPathRelFileP clientEntryPointVMId,
          "routesEntryPointVMId" .= toPathRelFileP routesEntryPointVMId,
          "ssrEntryPointVMId" .= toPathRelFileP ssrEntryPointVMId
        ]

genVirtualClientEntryTsx :: AppSpec -> Generator FileDraft
genVirtualClientEntryTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|client-entry.tsx|]
    tmplData =
      object
        [ "routeObjects" .= routeObjectsImportJson,
          "baseDir" .= SP.fromAbsDirP (WebApp.getBaseDir spec)
        ]

genVirtualSsrEntryTsx :: AppSpec -> Generator FileDraft
genVirtualSsrEntryTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|ssr-entry.tsx|]
    tmplData =
      object
        [ "routeObjects" .= routeObjectsImportJson,
          "spaFallbackFile" .= spaFallbackFile,
          "baseDir" .= SP.fromAbsDirP (WebApp.getBaseDir spec)
        ]

routeObjectsImportJson :: Value
routeObjectsImportJson =
  jsImportToImportJson $
    Just $
      makeValueJsImport (ModuleImportPath routesEntryPointVMId) (JsImportField "routeObjects")
