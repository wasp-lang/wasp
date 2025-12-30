module Wasp.Generator.SdkGenerator.Vite.VirtualFiles
  ( genVirtualFileTemplates,
    indexTsxFileName,
    routesFileName,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (isJust)
import StrongPath (reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.Common (dropExtensionFromImportPath)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as SDK
import Wasp.JsImport (JsImport (..), JsImportPath (..), applyJsImportAlias, getJsImportStmtAndIdentifier)

-- Virtual file names
indexTsxFileName :: String
indexTsxFileName = "index.generated.tsx"

routesFileName :: String
routesFileName = "routes.generated.tsx"

-- Data structures for routes template
data RoutesTemplateData = RoutesTemplateData
  { _routesIsAuthEnabled :: !Bool,
    _routesPagesToImport :: ![PageTemplateData],
    _routesRoutes :: ![RouteTemplateData]
  }

instance ToJSON RoutesTemplateData where
  toJSON routesTD =
    object
      [ "isAuthEnabled" .= _routesIsAuthEnabled routesTD,
        "pagesToImport" .= _routesPagesToImport routesTD,
        "routes" .= _routesRoutes routesTD
      ]

data RouteTemplateData = RouteTemplateData
  { _routeName :: !String,
    _routeTargetComponent :: !String
  }

instance ToJSON RouteTemplateData where
  toJSON routeTD =
    object
      [ "name" .= _routeName routeTD,
        "targetComponent" .= _routeTargetComponent routeTD
      ]

data PageTemplateData = PageTemplateData
  { _pageImportStmt :: !String
  }

instance ToJSON PageTemplateData where
  toJSON pageTD =
    object
      [ "importStatement" .= _pageImportStmt pageTD
      ]

-- | Generate virtual file templates that will be loaded by the virtualFiles plugin
genVirtualFileTemplates :: AppSpec -> Generator [FileDraft]
genVirtualFileTemplates spec = do
  return
    [ genVirtualIndexTsxTemplate spec,
      genVirtualRoutesTemplate spec,
      genVirtualIndexHtmlTemplate spec
    ]

genVirtualIndexTsxTemplate :: AppSpec -> FileDraft
genVirtualIndexTsxTemplate spec = SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = SDK.asTmplFile $ [reldir|client/vite/virtual-files|] </> [relfile|index.generated.ts|]
    tmplData =
      object
        [ "routesFileName" .= routesFileName,
          "hasAppComponent" .= isJust maybeRootComponent,
          "appComponentImport" .= getAppComponentImport maybeRootComponent,
          "hasClientSetup" .= isJust maybeSetupFn,
          "clientSetupImport" .= getClientSetupImport maybeSetupFn
        ]
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)
    maybeSetupFn = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)

genVirtualRoutesTemplate :: AppSpec -> FileDraft
genVirtualRoutesTemplate spec = SDK.mkTmplFdWithData tmplFile (toJSON templateData)
  where
    tmplFile = SDK.asTmplFile $ [reldir|client/vite/virtual-files|] </> [relfile|routes.generated.ts|]
    templateData = createRoutesTemplateData spec

genVirtualIndexHtmlTemplate :: AppSpec -> FileDraft
genVirtualIndexHtmlTemplate spec = SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = SDK.asTmplFile $ [reldir|client/vite/virtual-files|] </> [relfile|indexHtml.ts|]
    tmplData =
      object
        [ "htmlTitle" .= AS.App.title (snd $ getApp spec),
          "indexTsxFileName" .= indexTsxFileName
        ]

-- | Generate the import statement for the App component (rootComponent)
getAppComponentImport :: Maybe AS.ExtImport.ExtImport -> String
getAppComponentImport Nothing = ""
getAppComponentImport (Just extImport) =
  let jsImport = extImportToSdkSrcRelativeImport extImport "App"
      (importStmt, _) = getJsImportStmtAndIdentifier $ applyJsImportAlias (Just "App") jsImport
   in importStmt

-- | Generate the import statement for the client setup function
getClientSetupImport :: Maybe AS.ExtImport.ExtImport -> String
getClientSetupImport Nothing = ""
getClientSetupImport (Just extImport) =
  let jsImport = extImportToSdkSrcRelativeImport extImport "setup"
      (importStmt, _) = getJsImportStmtAndIdentifier $ applyJsImportAlias (Just "setup") jsImport
   in importStmt

-- | Convert ExtImport to JsImport with path relative to src directory
extImportToSdkSrcRelativeImport :: AS.ExtImport.ExtImport -> String -> JsImport
extImportToSdkSrcRelativeImport (AS.ExtImport.ExtImport extImportName extImportPath) alias =
  JsImport
    { _path = RelativeImportPath relativePath,
      _name = importName,
      _importAlias = Just (alias ++ "_ext")
    }
  where
    relativePathPrefix = [SP.reldirP|./src|]
    relativePath = dropExtensionFromImportPath $ relativePathPrefix </> SP.castRel extImportPath
    importName = GJI.extImportNameToJsImportName extImportName

-- Helper functions for creating template data
createRoutesTemplateData :: AppSpec -> RoutesTemplateData
createRoutesTemplateData spec =
  RoutesTemplateData
    { _routesIsAuthEnabled = isAuthEnabled spec,
      _routesPagesToImport = map createPageTemplateData $ AS.getPages spec,
      _routesRoutes = map (createRouteTemplateData spec) $ AS.getRoutes spec
    }

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> RouteTemplateData
createRouteTemplateData spec namedRoute@(name, _) =
  RouteTemplateData
    { _routeName = name,
      _routeTargetComponent = determineRouteTargetComponent spec namedRoute
    }

createPageTemplateData :: (String, AS.Page.Page) -> PageTemplateData
createPageTemplateData (pageName, page) =
  PageTemplateData
    { _pageImportStmt = importStmt
    }
  where
    importStmt :: String
    (importStmt, _) = getJsImportStmtAndIdentifier $ applyJsImportAlias (Just pageName) $ extImportToSdkSrcRelativeImport (AS.Page.component page) pageName

-- | Determine the target component for a route (with auth wrapping if needed)
determineRouteTargetComponent :: AppSpec -> (String, AS.Route.Route) -> String
determineRouteTargetComponent spec (_, route) =
  maybe
    targetPageName
    determineRouteTargetComponent'
    (AS.Page.authRequired $ snd targetPage)
  where
    targetPageName = AS.refName (AS.Route.to route :: AS.Ref AS.Page.Page)
    targetPage =
      case filter ((== targetPageName) . fst) (AS.getPages spec) of
        [page] -> page
        [] -> error $ "Can't find page with name '" ++ targetPageName ++ "'"
        _ -> error $ "Multiple pages with name '" ++ targetPageName ++ "'"

    determineRouteTargetComponent' :: Bool -> String
    determineRouteTargetComponent' authRequired =
      if authRequired
        then "createAuthRequiredPage(" ++ targetPageName ++ ")"
        else targetPageName
