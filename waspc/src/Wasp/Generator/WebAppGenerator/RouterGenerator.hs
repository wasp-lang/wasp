{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator.RouterGenerator
  ( genRouter,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (find)
import Data.Maybe (fromMaybe)
import StrongPath (Dir, Path, Rel, reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import StrongPath.Types (Posix)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.AuthProviders.OAuth (clientOAuthCallbackPath)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Common (asTmplFile, asWebAppSrcFile)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.JsImport (extImportToImportJson, extImportToJsImport)
import Wasp.JsImport (applyJsImportAlias, getDynamicJsImportExprAndIdentifier)

data RouterTemplateData = RouterTemplateData
  { _routes :: ![RouteTemplateData],
    _isAuthEnabled :: Bool,
    _isExternalAuthEnabled :: Bool,
    _rootComponent :: Aeson.Value,
    _baseDir :: String
  }

instance ToJSON RouterTemplateData where
  toJSON routerTD =
    object
      [ "routes" .= _routes routerTD,
        "isAuthEnabled" .= _isAuthEnabled routerTD,
        "isExternalAuthEnabled" .= _isExternalAuthEnabled routerTD,
        "rootComponent" .= _rootComponent routerTD,
        "baseDir" .= _baseDir routerTD,
        "oAuthCallbackPath" .= clientOAuthCallbackPath
      ]

data RouteTemplateData = RouteTemplateData
  { _routeName :: !String,
    _importExpr :: !String,
    _targetComponent :: !String,
    _isAuthRequired :: !Bool
  }

instance ToJSON RouteTemplateData where
  toJSON routeTD =
    object
      [ "name" .= _routeName routeTD,
        "importExpr" .= _importExpr routeTD,
        "targetComponent" .= _targetComponent routeTD,
        "isAuthRequired" .= _isAuthRequired routeTD
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

genRouter :: AppSpec -> Generator [FileDraft]
genRouter spec =
  sequence
    [ genRouterTsx spec
    ]

genRouterTsx :: AppSpec -> Generator FileDraft
genRouterTsx spec = do
  return $
    C.mkTmplFdWithDstAndData
      (asTmplFile $ [reldir|src|] </> routerPath)
      targetPath
      (Just $ toJSON templateData)
  where
    routerPath = [relfile|router.tsx|]
    templateData = createRouterTemplateData spec
    targetPath = C.webAppSrcDirInWebAppRootDir </> asWebAppSrcFile routerPath

createRouterTemplateData :: AppSpec -> RouterTemplateData
createRouterTemplateData spec =
  RouterTemplateData
    { _routes = routes,
      -- _pagesToImport = pages,
      _isAuthEnabled = isAuthEnabled spec,
      _isExternalAuthEnabled = (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True,
      _rootComponent = extImportToImportJson relPathToWebAppSrcDir maybeRootComponent,
      _baseDir = SP.fromAbsDirP $ C.getBaseDir spec
    }
  where
    routes = map (createRouteTemplateData spec) $ AS.getRoutes spec
    -- pages = map createPageTemplateData $ AS.getPages spec
    maybeAuth = AS.App.auth $ snd $ getApp spec
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> RouteTemplateData
createRouteTemplateData spec (name, route) =
  RouteTemplateData
    { _routeName = name,
      _importExpr = importExpr,
      _targetComponent = importIdentifier,
      _isAuthRequired = isAuthRequired
    }
  where
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
    isAuthRequired = AS.Page.authRequired (snd targetPage) == Just True

    (importExpr, importIdentifier) = getDynamicJsImportExprAndIdentifier $ applyJsImportAlias (Just importAlias) $ extImportToJsImport relPathToWebAppSrcDir pageComponent

    pageComponent = AS.Page.component $ snd targetPage

    importAlias = fst targetPage

relPathToWebAppSrcDir :: Path Posix (Rel importLocation) (Dir C.WebAppSrcDir)
relPathToWebAppSrcDir = [reldirP|./|]
