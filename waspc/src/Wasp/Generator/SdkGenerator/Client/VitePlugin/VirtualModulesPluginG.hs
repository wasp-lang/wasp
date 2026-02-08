{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPluginG
  ( getVirtualModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import StrongPath (relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (clientEntryPointPath, routesEntryPointPath, ssrEntryPointPath, virtualFilesDirInViteDir, virtualFilesFilesDirInViteDir)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified StrongPath as SP
import Wasp.Generator.WebAppGenerator.Common (getBaseDir)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPlugin.VirtualRoutesG (genVirtualRoutesTsx)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import qualified Wasp.Generator.SdkGenerator.Common as C

getVirtualModulesPlugin :: AppSpec -> Generator [FileDraft]
getVirtualModulesPlugin spec =
  sequence
    [ getVirtualModulesTs,
      genVirtualFilesResolverTs,
      genVirtualFilesIndexTs,
      genVirtualIndexTsx spec,
      genVirtualEntryServerTsx spec,
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
        [ "clientEntryPointPath" .= clientEntryPointPath,
          "routesEntryPointPath" .= routesEntryPointPath,
          "ssrEntryPointPath" .= ssrEntryPointPath
        ]

genVirtualIndexTsx :: AppSpec -> Generator FileDraft
genVirtualIndexTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|index.tsx|]
    tmplData =
      object
        [ "setupFn" .= GJI.jsImportToImportJson (GJI.extImportToRelativeSrcImportFromViteExecution <$> maybeSetupJsFunction),
          "rootComponent" .= rootComponentImportJson spec,
          "routesMapping" .= routesMappingImportJson,
          "routeNameToSsr" .= routeNameToSsrImportJson
        ]
    maybeSetupJsFunction = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)

genVirtualEntryServerTsx :: AppSpec -> Generator FileDraft
genVirtualEntryServerTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|entry-server.tsx|]
    tmplData =
      object
        [ "rootComponent" .= rootComponentImportJson spec,
          "routesMapping" .= routesMappingImportJson,
          "routeNameToSsr" .= routeNameToSsrImportJson,
          "routeNameToHead" .= routeNameToHeadImportJson,
          "isExternalAuthEnabled" .= isExternalAuthEnabled,
          "oAuthCallbackPath" .= clientOAuthCallbackPath,
          "baseDir" .= SP.fromAbsDirP (getBaseDir spec)
        ]
    maybeAuth = AS.App.auth $ snd $ getApp spec
    isExternalAuthEnabled = maybe False AS.Auth.isExternalAuthEnabled maybeAuth
    clientOAuthCallbackPath = OAuth.clientOAuthCallbackPath

-- Shared import JSON helpers for routes-related virtual module imports.

rootComponentImportJson :: AppSpec -> Aeson.Value
rootComponentImportJson spec =
  GJI.jsImportToImportJson (GJI.extImportToRelativeSrcImportFromViteExecution <$> maybeRootComponent)
  where
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)

routesMappingImportJson :: Aeson.Value
routesMappingImportJson =
  object
    [ "isDefined" .= True,
      "importStatement" .= ("import { routesMapping } from \"" ++ routesEntryPointPath ++ "\""),
      "importIdentifier" .= ("routesMapping" :: String)
    ]

routeNameToSsrImportJson :: Aeson.Value
routeNameToSsrImportJson =
  object
    [ "isDefined" .= True,
      "importStatement" .= ("import { routeNameToSsr } from \"" ++ routesEntryPointPath ++ "\""),
      "importIdentifier" .= ("routeNameToSsr" :: String)
    ]

routeNameToHeadImportJson :: Aeson.Value
routeNameToHeadImportJson =
  object
    [ "isDefined" .= True,
      "importStatement" .= ("import { routeNameToHead } from \"" ++ routesEntryPointPath ++ "\""),
      "importIdentifier" .= ("routeNameToHead" :: String)
    ]
