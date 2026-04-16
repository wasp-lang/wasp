{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPluginG
  ( getVirtualModulesPlugin,
  )
where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (maybeToList)
import StrongPath (relfile, toFilePath, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.AppSpec.Page as AS.Page
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (jsImportToImportJson)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (clientEntryPointPath, routesEntryPointPath, spaFallbackFile, ssrEntryPointPath, userClientEnvSchemaVF, userClientRootComponentVF, userClientSetupFnVF, userPageVF, virtualFilesDirInViteDir, virtualFilesFilesDirInViteDir)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPlugin.VirtualRoutesG (genVirtualRoutesTsx)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.JsImport (JsImportName (JsImportField), JsImportPath (RawImportName), VirtualFile, makeJsImport)

getVirtualModulesPlugin :: AppSpec -> Generator [FileDraft]
getVirtualModulesPlugin spec =
  sequence
    [ getWaspVirtualModulesTs,
      genUserVirtualModulesTs spec,
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
        [ "clientEntryPointPath" .= clientEntryPointPath,
          "routesEntryPointPath" .= routesEntryPointPath,
          "ssrEntryPointPath" .= ssrEntryPointPath
        ]

routeObjectsImportJson :: Value
routeObjectsImportJson =
  jsImportToImportJson $
    Just $
      makeJsImport (RawImportName routesEntryPointPath) (JsImportField "routeObjects")

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

genUserVirtualModulesTs :: AppSpec -> Generator FileDraft
genUserVirtualModulesTs spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|userVirtualModules.ts|]
    tmplData =
      object
        [ "userVirtualModules" .= getUserVFData spec
        ]

getUserVFData :: AppSpec -> [Aeson.Value]
getUserVFData spec =
  maybeToList (mkUserVFFromExtImport userClientEnvSchemaVF <$> maybeClientEnvSchema)
    ++ maybeToList (mkUserVFFromExtImport userClientSetupFnVF <$> maybeSetupFn)
    ++ maybeToList (mkUserVFFromExtImport userClientRootComponentVF <$> maybeRootComponent)
    ++ map mkPageVF (AS.getPages spec)
  where
    mkPageVF :: (String, AS.Page.Page) -> Aeson.Value
    mkPageVF (pageName, page) =
      mkUserVFFromExtImport (userPageVF pageName) (AS.Page.component page)

    mkUserVFFromExtImport :: VirtualFile -> EI.ExtImport -> Aeson.Value
    mkUserVFFromExtImport vf extImport =
      let jsImport = GJI.extImportToRelativeSrcImportFromViteExecution extImport
          importJson = GJI.jsImportToImportJson (Just jsImport)
       in object
            [ "virtualPath" .= toFilePath vf,
              "importJson" .= importJson
            ]

    maybeClientEnvSchema = AS.App.client app >>= AS.App.Client.envValidationSchema
    maybeSetupFn = AS.App.Client.setupFn =<< AS.App.client app
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client app
    app = snd $ getApp spec
