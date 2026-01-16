module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPlugin.VirtualRoutesG
  ( genVirtualRoutesTs,
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
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (virtualFilesDirInVitePluginsDir)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.JsImport
  ( applyJsImportAlias,
    getJsImportStmtAndIdentifier,
  )

genVirtualRoutesTs :: AppSpec -> Generator FileDraft
genVirtualRoutesTs spec =
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
    (importStmt, _) =
      getJsImportStmtAndIdentifier $
        applyJsImportAlias (Just pageName) $
          GJI.extImportToRelativeSrcImportFromViteExecution pageComponent

    pageComponent = AS.Page.component page
