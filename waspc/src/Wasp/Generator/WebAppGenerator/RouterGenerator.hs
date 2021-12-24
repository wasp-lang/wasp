{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator.RouterGenerator
  ( generateRouter,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import StrongPath (reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.WebAppGenerator.Common (asTmplFile, asWebAppSrcFile)
import qualified Wasp.Generator.WebAppGenerator.Common as C

data RouterTemplateData = RouterTemplateData
  { _routes :: ![RouteTemplateData],
    _pagesToImport :: ![PageTemplateData],
    _isAuthEnabled :: Bool
  }

instance ToJSON RouterTemplateData where
  toJSON routerTD =
    object
      [ "routes" .= _routes routerTD,
        "pagesToImport" .= _pagesToImport routerTD,
        "isAuthEnabled" .= _isAuthEnabled routerTD
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

generateRouter :: AppSpec -> FileDraft
generateRouter spec =
  C.makeTemplateFD
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
      _isAuthEnabled = isJust $ AS.App.auth $ snd $ AS.getApp spec
    }
  where
    routes = map (createRouteTemplateData spec) $ AS.getDecls @AS.Route.Route spec
    pages = map createPageTemplateData $ AS.getDecls @AS.Page.Page spec

-- TODO: Should I have something similar to WithCtx to describe that declarations are coming with a name?
-- So instead of (String, <some_decl>) it would be NamedDecl <some_decl> or WithName <some_decl> or smth like that?
createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> RouteTemplateData
createRouteTemplateData spec route =
  RouteTemplateData
    { _urlPath = AS.Route.path $ snd route,
      _targetComponent = determineRouteTargetComponent spec route
    }

determineRouteTargetComponent :: AppSpec -> (String, AS.Route.Route) -> String
determineRouteTargetComponent spec route =
  maybe
    targetPageName
    determineRouteTargetComponent'
    (AS.Page.authRequired $ snd targetPage)
  where
    targetPageName = AS.Core.Ref.refName $ AS.Route.to $ snd route
    targetPage =
      fromMaybe
        ( error $
            "Can't find page with name '" ++ targetPageName
              ++ "', pointed to by route '"
              ++ AS.Route.path (snd route)
              ++ "'"
        )
        (find ((==) targetPageName . fst) (AS.getDecls @AS.Page.Page spec))

    determineRouteTargetComponent' :: Bool -> String
    determineRouteTargetComponent' authRequired =
      if authRequired
        then -- TODO(matija): would be nicer if this function name wasn't hardcoded here.
          "createAuthRequiredPage(" ++ targetPageName ++ ")"
        else targetPageName

createPageTemplateData :: (String, AS.Page.Page) -> PageTemplateData
createPageTemplateData page =
  PageTemplateData
    { _importFrom =
        -- TODO: Once we make ExtImport.path be StrongPath, we will need to refactor here to use StrongPath.
        relPathToExtSrcDir ++ AS.ExtImport.path pageComponent,
      _importWhat = case AS.ExtImport.name pageComponent of
        AS.ExtImport.ExtImportModule _ -> pageName
        AS.ExtImport.ExtImportField identifier -> "{ " ++ identifier ++ " as " ++ pageName ++ " }"
    }
  where
    relPathToExtSrcDir :: FilePath
    relPathToExtSrcDir = "./ext-src/"

    pageName :: String
    pageName = fst page

    pageComponent :: AS.ExtImport.ExtImport
    pageComponent = AS.Page.component $ snd page
