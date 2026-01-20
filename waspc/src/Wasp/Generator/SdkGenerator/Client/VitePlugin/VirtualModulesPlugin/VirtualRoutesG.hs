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
  ( applyJsImportAlias,
    getJsImportStmtAndIdentifier,
  )

genVirtualRoutesTsx :: AppSpec -> Generator FileDraft
genVirtualRoutesTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|routes.tsx|]
    tmplData =
      object
        [ "routes" .= map (createRouteTemplateData spec) (AS.getRoutes spec),
          "pagesToImport" .= map createPageTemplateData (AS.getPages spec),
          "isAuthEnabled" .= isAuthEnabled spec
        ]

getRouteTargetComponent :: AppSpec -> (String, AS.Route.Route) -> String
getRouteTargetComponent spec (_, route) =
  if isAuthRequired
    then -- TODO(matija): would be nicer if this function name wasn't hardcoded here.
      "createAuthRequiredPage(" ++ targetPageName ++ ")"
    else targetPageName
  where
    isAuthRequired = fromMaybe False $ AS.Page.authRequired $ snd targetPage
    targetPageName = AS.refName (AS.Route.to route :: AS.Ref AS.Page.Page)
    targetPage =
      fromMaybe
        -- NOTE: This should be prevented by Analyzer, so use error since it should not be possible
        ( error $
            "Can't find page with name '"
              ++ targetPageName
              ++ "', pointed to by route '"
              ++ AS.Route.path route
              ++ "'"
        )
        (find ((==) targetPageName . fst) (AS.getPages spec))

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> Aeson.Value
createRouteTemplateData spec namedRoute@(name, _) =
  object
    [ "name" .= name,
      "targetComponent" .= getRouteTargetComponent spec namedRoute
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
