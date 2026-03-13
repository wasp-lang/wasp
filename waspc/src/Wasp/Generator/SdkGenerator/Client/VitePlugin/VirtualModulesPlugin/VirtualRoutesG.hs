module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPlugin.VirtualRoutesG
  ( genVirtualRoutesTsx,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (find)
import Data.Maybe (fromMaybe)
import StrongPath (relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (virtualFilesFilesDirInViteDir)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.JsImport
  ( JsImport (..),
    JsImportName (..),
    applyJsImportAlias,
    getImportIdentifier,
    getJsDynamicImportExpression,
    getJsImportPathString,
    getJsImportStmtAndIdentifier,
  )

genVirtualRoutesTsx :: AppSpec -> Generator FileDraft
genVirtualRoutesTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|routes.tsx|]
    routes = AS.getRoutes spec
    eagerRoutes = filter (not . isRouteLazy . snd) routes
    tmplData =
      object
        [ "routes" .= map (createRouteTemplateData spec) routes,
          "eagerImports" .= map (createEagerImportData spec) eagerRoutes,
          "isAuthEnabled" .= isAuthEnabled spec
        ]

isRouteLazy :: AS.Route.Route -> Bool
isRouteLazy = fromMaybe True . AS.Route.lazy

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> Aeson.Value
createRouteTemplateData spec (name, route) =
  object $
    [ "name" .= name,
      "isLazy" .= isLazy,
      "isAuthRequired" .= isAuthRequired
    ]
      ++ if isLazy
        then
          [ "importPath" .= getJsImportPathString jsImport,
            "importIdentifier" .= getImportIdentifier jsImport,
            "isDefaultExport" .= isDefaultExport,
            "dynamicImportExpression" .= getJsDynamicImportExpression jsImport
          ]
        else ["importIdentifier" .= importIdentifier]
  where
    isLazy = isRouteLazy route
    isAuthRequired = fromMaybe False $ AS.Page.authRequired $ snd targetPage

    targetPageName = AS.refName (AS.Route.to route :: AS.Ref AS.Page.Page)
    targetPage = findTargetPage spec targetPageName (AS.Route.path route)
    jsImport = GJI.extImportToRelativeSrcImportFromViteExecution $ AS.Page.component (snd targetPage)

    isDefaultExport = case _name jsImport of
      JsImportModule _ -> True
      JsImportField _ -> False

    -- For eager routes, we alias the import to the page name
    importIdentifier =
      let aliasedImport = applyJsImportAlias (Just targetPageName) jsImport
          (_, ident) = getJsImportStmtAndIdentifier aliasedImport
       in ident

createEagerImportData :: AppSpec -> (String, AS.Route.Route) -> Aeson.Value
createEagerImportData spec (_, route) =
  object ["importStatement" .= importStmt]
  where
    targetPageName = AS.refName (AS.Route.to route :: AS.Ref AS.Page.Page)
    targetPage = findTargetPage spec targetPageName (AS.Route.path route)
    jsImport = GJI.extImportToRelativeSrcImportFromViteExecution $ AS.Page.component (snd targetPage)
    aliasedImport = applyJsImportAlias (Just targetPageName) jsImport
    (importStmt, _) = getJsImportStmtAndIdentifier aliasedImport

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
