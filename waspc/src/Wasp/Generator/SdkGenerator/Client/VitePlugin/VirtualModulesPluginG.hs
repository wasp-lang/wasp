{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPluginG
  ( getVirtualModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (indexTsxVirtualFileName, routesTsxVirtualFileName, virtualFilesDirInVitePluginsDir)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPlugin.VirtualRoutesG (genVirtualRoutesTs)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.JsImport
  ( JsImportName (JsImportField),
    JsImportPath (RelativeImportPath),
    makeJsImport,
  )

getVirtualModulesPlugin :: AppSpec -> Generator [FileDraft]
getVirtualModulesPlugin spec =
  sequence
    [ getVirtualModulesTs,
      genVirtualIndexTs spec,
      genVirtualRoutesTs spec
    ]

getVirtualModulesTs :: Generator FileDraft
getVirtualModulesTs =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|virtualModules.ts|]
    tmplData =
      object
        [ "indexVirtualFileName" .= indexTsxVirtualFileName,
          "routesVirtualFileName" .= routesTsxVirtualFileName
        ]

genVirtualIndexTs :: AppSpec -> Generator FileDraft
genVirtualIndexTs spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> virtualFilesDirInVitePluginsDir </> [relfile|index.virtual.ts|]
    tmplData =
      object
        [ "setupFn" .= GJI.jsImportToImportJson (GJI.extImportToRelativeSrcImportFromViteExecution <$> maybeSetupJsFunction),
          "rootComponent" .= GJI.jsImportToImportJson (GJI.extImportToRelativeSrcImportFromViteExecution <$> maybeRootComponent),
          "routesMapping" .= GJI.jsImportToImportJson (Just routesMappingJsImport)
        ]
    maybeSetupJsFunction = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)
    routesMappingJsImport =
      makeJsImport
        (RelativeImportPath $ fromJust $ SP.parseRelFileP $ "./" ++ routesTsxVirtualFileName)
        (JsImportField "routesMapping")
