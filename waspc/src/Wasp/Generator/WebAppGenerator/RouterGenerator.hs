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
import StrongPath.Types (Posix)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.AuthProviders (gitHubAuthInfo, googleAuthInfo)
import Wasp.Generator.AuthProviders.OAuth (OAuthAuthInfo, frontendLoginUrl, serverOauthRedirectHandlerUrl)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Common (asTmplFile, asWebAppSrcFile)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.JsImport (extImportToImportJson, extImportToJsImport)
import Wasp.JsImport (applyJsImportAlias, getJsImportStmtAndIdentifier)

data RouterTemplateData = RouterTemplateData
  { _routes :: ![RouteTemplateData],
    _pagesToImport :: ![PageTemplateData],
    _isAuthEnabled :: Bool,
    _isExternalAuthEnabled :: Bool,
    _externalAuthProviders :: ![ExternalAuthProviderTemplateData],
    _rootComponent :: Aeson.Value
  }

instance ToJSON RouterTemplateData where
  toJSON routerTD =
    object
      [ "routes" .= _routes routerTD,
        "pagesToImport" .= _pagesToImport routerTD,
        "isAuthEnabled" .= _isAuthEnabled routerTD,
        "isExternalAuthEnabled" .= _isExternalAuthEnabled routerTD,
        "externalAuthProviders" .= _externalAuthProviders routerTD,
        "rootComponent" .= _rootComponent routerTD
      ]

data RouteTemplateData = RouteTemplateData
  { _urlPath :: !String,
    _targetComponent :: !String
  }

instance ToJSON RouteTemplateData where
  toJSON routeTD =
    object
      [ "urlPath" .= _urlPath routeTD,
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

data ExternalAuthProviderTemplateData = ExternalAuthProviderTemplateData
  { _authFrontendUrl :: !String,
    _authServerOauthRedirectUrl :: !String,
    _authProviderEnabled :: Bool
  }
  deriving (Show, Eq)

instance ToJSON ExternalAuthProviderTemplateData where
  toJSON externalProviderTD =
    object
      [ "authFrontendUrl" .= _authFrontendUrl externalProviderTD,
        "authServerOauthRedirectUrl" .= _authServerOauthRedirectUrl externalProviderTD,
        "authProviderEnabled" .= _authProviderEnabled externalProviderTD
      ]

genRouter :: AppSpec -> Generator FileDraft
genRouter spec = do
  return $
    C.mkTmplFdWithDstAndData
      (asTmplFile $ [reldir|src|] </> routerPath)
      targetPath
      (Just $ toJSON templateData)
  where
    routerPath = [relfile|router.jsx|]
    templateData = createRouterTemplateData spec
    targetPath = C.webAppSrcDirInWebAppRootDir </> asWebAppSrcFile routerPath

createRouterTemplateData :: AppSpec -> RouterTemplateData
createRouterTemplateData spec =
  RouterTemplateData
    { _routes = routes,
      _pagesToImport = pages,
      _isAuthEnabled = isAuthEnabled spec,
      _isExternalAuthEnabled = (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True,
      _externalAuthProviders = externalAuthProviders,
      _rootComponent = extImportToImportJson relPathToWebAppSrcDir maybeRootComponent
    }
  where
    routes = map (createRouteTemplateData spec) $ AS.getRoutes spec
    pages = map createPageTemplateData $ AS.getPages spec
    externalAuthProviders =
      map
        (createExternalAuthProviderTemplateData maybeAuth)
        [ (AS.App.Auth.isGoogleAuthEnabled, googleAuthInfo),
          (AS.App.Auth.isGitHubAuthEnabled, gitHubAuthInfo)
        ]
    maybeAuth = AS.App.auth $ snd $ getApp spec
    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)

createExternalAuthProviderTemplateData ::
  Maybe AS.App.Auth.Auth ->
  (AS.App.Auth.Auth -> Bool, OAuthAuthInfo) ->
  ExternalAuthProviderTemplateData
createExternalAuthProviderTemplateData maybeAuth (method, oAuthAuthInfo) =
  ExternalAuthProviderTemplateData
    { _authFrontendUrl = frontendLoginUrl oAuthAuthInfo,
      _authServerOauthRedirectUrl = serverOauthRedirectHandlerUrl oAuthAuthInfo,
      _authProviderEnabled = (method <$> maybeAuth) == Just True
    }

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> RouteTemplateData
createRouteTemplateData spec namedRoute@(_, route) =
  RouteTemplateData
    { _urlPath = AS.Route.path route,
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
            "Can't find page with name '" ++ targetPageName
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
createPageTemplateData page =
  PageTemplateData
    { _importStmt = importStmt
    }
  where
    importStmt :: String
    (importStmt, _) = getJsImportStmtAndIdentifier $ applyJsImportAlias (Just importAlias) $ extImportToJsImport relPathToWebAppSrcDir pageComponent

    pageComponent :: AS.ExtImport.ExtImport
    pageComponent = AS.Page.component $ snd page

    importAlias :: String
    importAlias = fst page

relPathToWebAppSrcDir :: Path Posix (Rel importLocation) (Dir C.WebAppSrcDir)
relPathToWebAppSrcDir = [reldirP|./|]
