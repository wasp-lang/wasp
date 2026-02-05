module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPlugin.VirtualRoutesG
  ( genVirtualRoutesTsx,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (find, stripPrefix)
import Data.Maybe (fromMaybe)
import StrongPath (relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.ExtImport as EI
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
getRouteTargetComponent spec namedRoute =
  if isAuthRequired
    then -- TODO(matija): would be nicer if this function name wasn't hardcoded here.
      "createAuthRequiredPage(" ++ targetComponentExpr ++ ")"
    else targetComponentExpr
  where
    (targetPageName, targetPage) = getTargetPage spec namedRoute
    targetComponentExpr = getPageComponentExpr targetPageName targetPage
    isAuthRequired = fromMaybe False $ AS.Page.authRequired targetPage

getPageComponentExpr :: String -> AS.Page.Page -> String
getPageComponentExpr pageModuleIdentifier page =
  case EI.name $ AS.Page.component page of
    EI.ExtImportModule _ -> pageModuleIdentifier ++ ".default"
    EI.ExtImportField exportedName -> pageModuleIdentifier ++ "." ++ exportedName

getTargetPage :: AppSpec -> (String, AS.Route.Route) -> (String, AS.Page.Page)
getTargetPage spec (_, route) =
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
  where
    targetPageName = AS.refName (AS.Route.to route :: AS.Ref AS.Page.Page)

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> Aeson.Value
createRouteTemplateData spec namedRoute@(name, _) =
  object
    [ "name" .= name,
      "targetComponent" .= getRouteTargetComponent spec namedRoute,
      "ssr" .= (fromMaybe False $ AS.Page.ssr targetPage),
      "pageModuleIdentifier" .= targetPageName
    ]
  where
    (targetPageName, targetPage) = getTargetPage spec namedRoute

createPageTemplateData :: (String, AS.Page.Page) -> Aeson.Value
createPageTemplateData (pageName, page) =
  object
    [ "importStatement" .= importStmt
    ]
  where
    importStmt :: String
    (defaultImportStmt, _) =
      getJsImportStmtAndIdentifier $
        applyJsImportAlias (Just pageName) $
          GJI.extImportToRelativeSrcImportFromViteExecution pageComponent
    importStmt = toNamespaceImport defaultImportStmt pageName

    pageComponent = AS.Page.component page

toNamespaceImport :: String -> String -> String
toNamespaceImport importStmt importIdentifier =
  case stripPrefix "import " importStmt of
    Just rest -> case extractPath rest of
      Just path -> "import * as " ++ importIdentifier ++ " from '" ++ path ++ "'"
      Nothing -> importStmt
    Nothing -> importStmt

extractPath :: String -> Maybe String
extractPath stmt =
  case dropWhile (/= '\'') stmt of
    '\'' : rest -> Just (takeWhile (/= '\'') rest)
    _ -> Nothing
