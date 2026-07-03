{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPluginG
  ( getVirtualModulesPlugin,
  )
where

import Data.Aeson (Value, object, (.=))
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (clientEntryPointVMId, routesEntryPointVMId, spaFallbackFile, ssrEntryPointVMId, virtualFilesDirInViteDir, virtualFilesFilesDirInViteDir)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPlugin.VirtualRoutesG (genVirtualRoutesTsx)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.JsImport (JsImportName (JsImportField), JsImportPath (ModuleImportPath), makeValueJsImport)

getVirtualModulesPlugin :: AppSpec -> Generator [FileDraft]
getVirtualModulesPlugin spec =
  sequence
    [ getVirtualModulesTs,
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

getVirtualModulesTs :: Generator FileDraft
getVirtualModulesTs =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|virtualModules.ts|]
    tmplData =
      object
        [ "clientEntryPointVMId" .= SP.fromRelFileP clientEntryPointVMId,
          "routesEntryPointVMId" .= SP.fromRelFileP routesEntryPointVMId,
          "ssrEntryPointVMId" .= SP.fromRelFileP ssrEntryPointVMId
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
          "spaFallbackFile" .= SP.fromRelFileP spaFallbackFile,
          "baseDir" .= SP.fromAbsDirP (WebApp.getBaseDir spec)
        ]

routeObjectsImportJson :: Value
routeObjectsImportJson =
  jsImportToImportJson $
    Just $
      makeValueJsImport (ModuleImportPath routesEntryPointVMId) (JsImportField "routeObjects")
