{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPluginG
  ( getVirtualModulesPlugin,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (indexTsxVirtualFileName, routesTsxVirtualFileName, virtualFilesDirInVitePluginsDir)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.SdkGenerator.JsImport as SJ
import Wasp.JsImport
  ( JsImportName (JsImportField),
    JsImportPath (RelativeImportPath),
    applyJsImportAlias,
    getJsImportStmtAndIdentifier,
    makeJsImport,
  )

getVirtualModulesPlugin :: AppSpec -> Generator [FileDraft]
getVirtualModulesPlugin spec =
  sequence
    [ getVirtualModulesTs,
      genVirtualIndexTsx spec,
      genVirtualRoutesTsx spec
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

genVirtualIndexTsx :: AppSpec -> Generator FileDraft
genVirtualIndexTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> virtualFilesDirInVitePluginsDir </> [relfile|index.virtual.ts|]
    tmplData =
      object
        [ "setupFn" .= GJI.jsImportToImportJson (SJ.extImportToSdkSrcRelativeImport <$> maybeSetupJsFunction),
          "rootComponent" .= GJI.jsImportToImportJson (SJ.extImportToSdkSrcRelativeImport <$> maybeRootComponent),
          "routesMapping" .= GJI.jsImportToImportJson (Just routesMappingJsImport)
        ]
    maybeSetupJsFunction = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)
    routesMappingJsImport =
      makeJsImport
        (RelativeImportPath $ fromJust $ SP.parseRelFileP $ "./" ++ routesTsxVirtualFileName)
        (JsImportField "routesMapping")

genVirtualRoutesTsx :: AppSpec -> Generator FileDraft
genVirtualRoutesTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> virtualFilesDirInVitePluginsDir </> [relfile|routes.virtual.ts|]
    tmplData =
      object
        [ "routes" .= map (createRouteTemplateData spec) (AS.getRoutes spec),
          "pagesToImport" .= map createPageTemplateData (AS.getPages spec),
          "isAuthEnabled" .= isAuthEnabled spec
        ]

-- NOTE: This should be prevented by Analyzer, so use error since it should not be possible
determineRouteTargetComponent :: AppSpec -> (String, AS.Route.Route) -> String
determineRouteTargetComponent spec (_, route) =
  maybe
    targetPageName
    determineRouteTargetComponent'
    (AS.Page.authRequired $ snd targetPage)
  where
    targetPageName = AS.refName (AS.Route.to route :: AS.Ref AS.Page.Page)
    targetPage =
      fromMaybe
        ( error $
            "Can't find page with name '"
              ++ targetPageName
              ++ "', pointed to by route '"
              ++ AS.Route.path route
              ++ "'"
        )
        (find ((==) targetPageName . fst) (AS.getPages spec))

    determineRouteTargetComponent' :: Bool -> String
    determineRouteTargetComponent' authRequired =
      if authRequired
        then -- TODO(matija): would be nicer if this function name wasn't hardcoded here.
          "createAuthRequiredPage(" ++ targetPageName ++ ")"
        else targetPageName

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> Aeson.Value
createRouteTemplateData spec namedRoute@(name, _) =
  object
    [ "name" .= name,
      "targetComponent" .= determineRouteTargetComponent spec namedRoute
    ]

createPageTemplateData :: (String, AS.Page.Page) -> Aeson.Value
createPageTemplateData (pageName, page) =
  object
    [ "importStatement" .= importStmt
    ]
  where
    importStmt :: String
    (importStmt, _) = getJsImportStmtAndIdentifier $ applyJsImportAlias (Just pageName) $ SJ.extImportToSdkSrcRelativeImport pageComponent

    pageComponent = AS.Page.component page
