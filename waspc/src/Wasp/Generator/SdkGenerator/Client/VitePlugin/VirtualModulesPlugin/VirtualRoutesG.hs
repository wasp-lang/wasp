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
import Wasp.Generator.JsImport (extImportToRelativeSrcImportFromViteExecution, jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (virtualFilesFilesDirInViteDir)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.JsImport (applyJsImportAlias)

genVirtualRoutesTsx :: AppSpec -> Generator FileDraft
genVirtualRoutesTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|routes.tsx|]
    tmplData =
      object
        [ "routes" .= map (createRouteTemplateData spec) (AS.getRoutes spec),
          "isAuthEnabled" .= isAuthEnabled spec
        ]

isRouteLazy :: AS.Route.Route -> Bool
isRouteLazy = fromMaybe True . AS.Route.lazy

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> Aeson.Value
createRouteTemplateData spec (name, route) =
  object
    [ "name" .= name,
      "isLazy" .= isRouteLazy route,
      "isAuthRequired" .= isAuthRequired,
      "import" .= jsImportToImportJson (Just aliasedImport)
    ]
  where
    isAuthRequired = fromMaybe False $ AS.Page.authRequired $ snd targetPage

    targetPageName = AS.refName (AS.Route.to route :: AS.Ref AS.Page.Page)
    targetPage = findTargetPage spec targetPageName (AS.Route.path route)
    jsImport = extImportToRelativeSrcImportFromViteExecution $ AS.Page.component (snd targetPage)
    aliasedImport = applyJsImportAlias (Just targetPageName) jsImport

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
