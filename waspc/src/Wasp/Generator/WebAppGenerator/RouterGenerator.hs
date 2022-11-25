{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator.RouterGenerator
  ( genRouter,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.List (find)
import Data.Maybe (fromMaybe)
import StrongPath (reldir, relfile, (</>))
import qualified StrongPath as SP
import qualified System.FilePath as FP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Common (asTmplFile, asWebAppSrcFile)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator (extClientCodeDirInWebAppSrcDir)

data RouterTemplateData = RouterTemplateData
  { _routes :: ![RouteTemplateData],
    _pagesToImport :: ![PageTemplateData],
    _isAuthEnabled :: Bool,
    _isExternalAuthEnabled :: Bool,
    _isGoogleAuthEnabled :: Bool
  }

instance ToJSON RouterTemplateData where
  toJSON routerTD =
    object
      [ "routes" .= _routes routerTD,
        "pagesToImport" .= _pagesToImport routerTD,
        "isAuthEnabled" .= _isAuthEnabled routerTD,
        "isExternalAuthEnabled" .= _isExternalAuthEnabled routerTD,
        "isGoogleAuthEnabled" .= _isGoogleAuthEnabled routerTD
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
  { _importWhat :: !String,
    _importFrom :: !String
  }
  deriving (Show, Eq)

instance ToJSON PageTemplateData where
  toJSON pageTD =
    object
      [ "importWhat" .= _importWhat pageTD,
        "importFrom" .= _importFrom pageTD
      ]

genRouter :: AppSpec -> Generator FileDraft
genRouter spec = do
  return $
    C.mkTmplFdWithDstAndData
      (asTmplFile $ [reldir|src|] </> routerPath)
      targetPath
      (Just $ toJSON templateData)
  where
    routerPath = [relfile|router.js|]
    templateData = createRouterTemplateData spec
    targetPath = C.webAppSrcDirInWebAppRootDir </> asWebAppSrcFile routerPath

createRouterTemplateData :: AppSpec -> RouterTemplateData
createRouterTemplateData spec =
  RouterTemplateData
    { _routes = routes,
      _pagesToImport = pages,
      _isAuthEnabled = isAuthEnabled spec,
      _isExternalAuthEnabled = (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True,
      _isGoogleAuthEnabled = (AS.App.Auth.isGoogleAuthEnabled <$> maybeAuth) == Just True
    }
  where
    routes = map (createRouteTemplateData spec) $ AS.getRoutes spec
    pages = map createPageTemplateData $ AS.getPages spec
    maybeAuth = AS.App.auth $ snd $ getApp spec

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
    { _importFrom = relPathToExtSrcDir FP.</> SP.fromRelFileP (AS.ExtImport.path pageComponent),
      _importWhat = case AS.ExtImport.name pageComponent of
        AS.ExtImport.ExtImportModule _ -> pageName
        AS.ExtImport.ExtImportField identifier -> "{ " ++ mkNamedImportExpr identifier pageName ++ " }"
    }
  where
    relPathToExtSrcDir = "./" FP.</> SP.toFilePath extClientCodeDirInWebAppSrcDir

    pageName :: String
    pageName = fst page

    pageComponent :: AS.ExtImport.ExtImport
    pageComponent = AS.Page.component $ snd page

mkNamedImportExpr :: String -> String -> String
mkNamedImportExpr identifier alias
  | identifier == alias = identifier
  | otherwise = identifier ++ " as " ++ alias
