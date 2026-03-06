{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPluginG
  ( getVirtualModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (maybeToList)
import StrongPath (relfile, toFilePath, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.AppSpec.Page as AS.Page
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (clientEntryPointPath, pageVF, routesEntryPointPath, userClientEnvSchemaVF, userRootComponentVF, userSetupFnVF, virtualFilesDirInViteDir, virtualFilesFilesDirInViteDir)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPlugin.VirtualRoutesG (genVirtualRoutesTsx)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.JsImport (VirtualFile)

getVirtualModulesPlugin :: AppSpec -> Generator [FileDraft]
getVirtualModulesPlugin spec =
  sequence
    [ getVirtualModulesTs spec,
      genVirtualFilesResolverTs,
      genVirtualFilesIndexTs,
      genVirtualIndexTsx spec,
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

getVirtualModulesTs :: AppSpec -> Generator FileDraft
getVirtualModulesTs spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|virtualModules.ts|]
    tmplData =
      object
        [ "clientEntryPointPath" .= clientEntryPointPath,
          "routesEntryPointPath" .= routesEntryPointPath,
          "directVirtualModules" .= directVirtualModules,
          "hasDirectVirtualModules" .= (not . null $ directVirtualModules)
        ]

    directVirtualModules :: [Aeson.Value]
    directVirtualModules =
      maybeToList (mkDirectVirtualModule userClientEnvSchemaVF <$> maybeClientEnvSchema)
        ++ maybeToList (mkDirectVirtualModule userSetupFnVF <$> maybeSetupFn)
        ++ maybeToList (mkDirectVirtualModule userRootComponentVF <$> maybeRootComponent)
        ++ map mkPageVirtualModule (AS.getPages spec)

    maybeClientEnvSchema = AS.App.client (snd $ getApp spec) >>= AS.App.Client.envValidationSchema
    maybeSetupFn = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)

    mkDirectVirtualModule :: VirtualFile -> EI.ExtImport -> Aeson.Value
    mkDirectVirtualModule vf extImport =
      let jsImport = GJI.extImportToRelativeSrcImportFromViteExecution extImport
          importJson = GJI.jsImportToImportJson (Just jsImport)
       in object
            [ "virtualPath" .= toFilePath vf,
              "importJson" .= importJson
            ]

    mkPageVirtualModule :: (String, AS.Page.Page) -> Aeson.Value
    mkPageVirtualModule (pageName, page) =
      mkDirectVirtualModule (pageVF pageName) (AS.Page.component page)

genVirtualIndexTsx :: AppSpec -> Generator FileDraft
genVirtualIndexTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|index.tsx|]
    tmplData =
      object
        [ "setupFn" .= GJI.virtualExtImportToImportJson userSetupFnVF maybeSetupJsFunction,
          "rootComponent" .= GJI.virtualExtImportToImportJson userRootComponentVF maybeRootComponent,
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

