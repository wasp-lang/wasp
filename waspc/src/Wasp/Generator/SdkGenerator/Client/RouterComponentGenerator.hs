{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.RouterComponentGenerator
  ( genClientRouterComponent,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (find)
import Data.Maybe (fromMaybe)
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.AuthProviders.OAuth (clientOAuthCallbackPath)
import Wasp.Generator.Common (dropExtensionFromImportPath)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.Common as WebAppC
import Wasp.JsImport (JsImport (..), JsImportPath (..), applyJsImportAlias, getJsImportStmtAndIdentifier)

data RouterTemplateData = RouterTemplateData
  { _routes :: ![RouteTemplateData],
    _pagesToImport :: ![PageTemplateData],
    _isAuthEnabled :: Bool,
    _isExternalAuthEnabled :: Bool,
    _rootComponent :: Aeson.Value,
    _baseDir :: String
  }

instance ToJSON RouterTemplateData where
  toJSON routerTD =
    object
      [ "routes" .= _routes routerTD,
        "pagesToImport" .= _pagesToImport routerTD,
        "isAuthEnabled" .= _isAuthEnabled routerTD,
        "isExternalAuthEnabled" .= _isExternalAuthEnabled routerTD,
        "rootComponent" .= _rootComponent routerTD,
        "baseDir" .= _baseDir routerTD,
        "oAuthCallbackPath" .= clientOAuthCallbackPath
      ]

data RouteTemplateData = RouteTemplateData
  { _routeName :: !String,
    _targetComponent :: !String
  }

instance ToJSON RouteTemplateData where
  toJSON routeTD =
    object
      [ "name" .= _routeName routeTD,
        "targetComponent" .= _targetComponent routeTD
      ]

data PageTemplateData = PageTemplateData
  { _importStmt :: !String
  }
  deriving (Show, Eq)

instance ToJSON PageTemplateData where
  toJSON pageTD =
    object
      [ "importStatement" .= _importStmt pageTD
      ]

genClientRouterComponent :: AppSpec -> Generator [FileDraft]
genClientRouterComponent spec =
  sequence
    [ genRouterTsx spec
    ]

genRouterTsx :: AppSpec -> Generator FileDraft
genRouterTsx spec = do
  return $
    C.mkTmplFdWithData
      [relfile|client/router/router.tsx|]
      (toJSON templateData)
  where
    templateData = createRouterTemplateData spec

createRouterTemplateData :: AppSpec -> RouterTemplateData
createRouterTemplateData spec =
  RouterTemplateData
    { _routes = routes,
      _pagesToImport = pages,
      _isAuthEnabled = isAuthEnabled spec,
      _isExternalAuthEnabled = (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True,
      _rootComponent = GJI.jsImportToImportJson $ extImportToSdkSrcRelativeImport <$> maybeRootComponent,
      _baseDir = SP.fromAbsDirP $ WebAppC.getBaseDir spec
    }
  where
    routes = map (createRouteTemplateData spec) $ AS.getRoutes spec
    pages = map createPageTemplateData $ AS.getPages spec
    maybeAuth = AS.App.auth $ snd $ getApp spec
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> RouteTemplateData
createRouteTemplateData spec namedRoute@(name, _) =
  RouteTemplateData
    { _routeName = name,
      _targetComponent = determineRouteTargetComponent spec namedRoute
    }

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

createPageTemplateData :: (String, AS.Page.Page) -> PageTemplateData
createPageTemplateData (pageName, page) =
  PageTemplateData
    { _importStmt = importStmt
    }
  where
    importStmt :: String
    (importStmt, _) = getJsImportStmtAndIdentifier $ applyJsImportAlias (Just pageName) $ extImportToSdkSrcRelativeImport $ AS.Page.component page

extImportToSdkSrcRelativeImport :: AS.ExtImport.ExtImport -> JsImport
extImportToSdkSrcRelativeImport extImport@(AS.ExtImport.ExtImport extImportName extImportPath) =
  JsImport
    { _path = RelativeImportPath relativePath,
      _name = importName,
      _importAlias = Just $ AS.ExtImport.importIdentifier extImport ++ "_ext"
    }
  where
    relativePathPrefix = [SP.reldirP|../../src|]
    relativePath = dropExtensionFromImportPath $ relativePathPrefix </> SP.castRel extImportPath
    importName = GJI.extImportNameToJsImportName extImportName
