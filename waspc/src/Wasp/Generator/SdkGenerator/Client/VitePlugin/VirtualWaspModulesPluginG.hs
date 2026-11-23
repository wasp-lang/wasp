{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualWaspModulesPluginG
  ( genVirtualWaspModulesPlugin,
  )
where

import Data.Aeson (Value, object, (.=))
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (extImportToRelativeSrcImportFromViteExecution, jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (clientEntryPointPath, routesEntryPointPath, spaFallbackFile, ssrEntryPointPath, virtualFilesDirInViteDir, virtualFilesFilesDirInViteDir)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualWaspModulesPlugin.VirtualRoutesG (genVirtualRoutesTsx)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.JsImport (JsImportName (JsImportField), JsImportPath (RawImportName), makeValueJsImport)
import Wasp.Util.Js (makeJsStringLiteral)

genVirtualWaspModulesPlugin :: AppSpec -> Generator [FileDraft]
genVirtualWaspModulesPlugin spec =
  sequence
    [ genVirtualWaspModulesTs,
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

genVirtualWaspModulesTs :: Generator FileDraft
genVirtualWaspModulesTs =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|virtualWaspModules.ts|]
    tmplData =
      object
        [ "clientEntryPointPath" .= clientEntryPointPath,
          "routesEntryPointPath" .= routesEntryPointPath,
          "ssrEntryPointPath" .= ssrEntryPointPath
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
          "onErrorFn" .= jsImportToImportJson (extImportToRelativeSrcImportFromViteExecution <$> maybeOnErrorJsFunction),
          "baseDir" .= makeJsStringLiteral (SP.fromAbsDirP (WebApp.getBaseDir spec))
        ]
    maybeOnErrorJsFunction = AS.App.Client.onError =<< AS.App.client (getApp spec)

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
          "baseDir" .= makeJsStringLiteral (SP.fromAbsDirP (WebApp.getBaseDir spec))
        ]

routeObjectsImportJson :: Value
routeObjectsImportJson =
  jsImportToImportJson
    $ Just
    $ makeValueJsImport (RawImportName routesEntryPointPath) (JsImportField "routeObjects")
