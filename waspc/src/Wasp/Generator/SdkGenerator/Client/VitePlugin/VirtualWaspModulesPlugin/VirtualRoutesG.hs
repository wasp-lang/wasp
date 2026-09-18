module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualWaspModulesPlugin.VirtualRoutesG
  ( genVirtualRoutesTsx,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (find, nub)
import Data.Maybe (fromMaybe)
import StrongPath (relfile, (</>))
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
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (virtualFilesFilesDirInViteDir)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.JsImport (JsImport, applyJsImportAlias)

genVirtualRoutesTsx :: AppSpec -> Generator FileDraft
genVirtualRoutesTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|routes.tsx|]
    tmplData =
      object
        [ "routes" .= map (createRouteTemplateData spec) (AS.getRoutes spec),
          "eagerImports" .= eagerRouteImportTemplateData,
          "isAuthEnabled" .= isAuthEnabled spec,
          "setupFn" .= GJI.jsImportToImportJson (GJI.extImportToRelativeSrcImportFromViteExecution <$> maybeSetupJsFunction),
          "rootComponent" .= GJI.jsImportToImportJson (GJI.extImportToRelativeSrcImportFromViteExecution <$> maybeRootComponent)
        ]
    maybeSetupJsFunction = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)
    -- Eager route imports are rendered at the top level of the generated
    -- module, so importing the same page through multiple routes must emit a
    -- single import statement. Otherwise the generated module contains
    -- duplicate declarations and fails to parse.
    eagerRouteImportTemplateData =
      map (GJI.jsImportToImportJson . Just) $
        nub [routePageImport spec route | (_, route) <- AS.getRoutes spec, not (isRouteLazy route)]

isRouteLazy :: AS.Route.Route -> Bool
isRouteLazy = fromMaybe True . AS.Route.lazy

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> Aeson.Value
createRouteTemplateData spec (name, route) =
  object
    [ "name" .= name,
      "isLazy" .= isRouteLazy route,
      "isAuthRequired" .= isAuthRequired,
      "import" .= GJI.jsImportToImportJson (Just $ routePageImport spec route)
    ]
  where
    isAuthRequired = fromMaybe False $ AS.Page.authRequired $ snd targetPage

    targetPageName = AS.refName (AS.Route.to route :: AS.Ref AS.Page.Page)
    targetPage = findTargetPage spec targetPageName (AS.Route.path route)

routePageImport :: AppSpec -> AS.Route.Route -> JsImport
routePageImport spec route =
  applyJsImportAlias (Just targetPageName) jsImport
  where
    targetPageName = AS.refName (AS.Route.to route :: AS.Ref AS.Page.Page)
    targetPage = findTargetPage spec targetPageName (AS.Route.path route)
    jsImport = GJI.extImportToRelativeSrcImportFromViteExecution $ AS.Page.component (snd targetPage)

findTargetPage :: AppSpec -> String -> String -> (String, AS.Page.Page)
findTargetPage spec targetPageName routePath =
  fromMaybe
    ( error $
        "Can't find page with name '"
          ++ targetPageName
          ++ "', pointed to by route '"
          ++ routePath
          ++ "'"
    )
    (find ((==) targetPageName . fst) (AS.getPages spec))
