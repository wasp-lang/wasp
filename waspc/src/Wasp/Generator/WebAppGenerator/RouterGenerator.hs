module Wasp.Generator.WebAppGenerator.RouterGenerator
  ( generateRouter,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe, isJust)
import StrongPath (reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.WebAppGenerator.Common (asTmplFile, asWebAppSrcFile)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Wasp (Wasp)
import qualified Wasp.Wasp as Wasp
import qualified Wasp.Wasp.JsImport as Wasp.JsImport
import qualified Wasp.Wasp.Page as Wasp.Page
import qualified Wasp.Wasp.Route as Wasp.Route

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

generateRouter :: Wasp -> FileDraft
generateRouter wasp =
  C.makeTemplateFD
    (asTmplFile $ [reldir|src|] </> routerPath)
    targetPath
    (Just $ toJSON templateData)
  where
    routerPath = [relfile|router.js|]
    templateData = createRouterTemplateData wasp
    targetPath = C.webAppSrcDirInWebAppRootDir </> asWebAppSrcFile routerPath

createRouterTemplateData :: Wasp -> RouterTemplateData
createRouterTemplateData wasp =
  RouterTemplateData
    { _routes = routes,
      _pagesToImport = pages,
      _isAuthEnabled = isJust $ Wasp.getAuth wasp
    }
  where
    routes = map (createRouteTemplateData wasp) $ Wasp.getRoutes wasp
    pages = map createPageTemplateData $ Wasp.getPages wasp

createRouteTemplateData :: Wasp -> Wasp.Route.Route -> RouteTemplateData
createRouteTemplateData wasp route =
  RouteTemplateData
    { _urlPath = Wasp.Route._urlPath route,
      _targetComponent = determineRouteTargetComponent wasp route
    }

determineRouteTargetComponent :: Wasp -> Wasp.Route.Route -> String
determineRouteTargetComponent wasp route =
  maybe
    targetPageName
    determineRouteTargetComponent'
    (Wasp.Page._authRequired targetPage)
  where
    targetPageName = Wasp.Route._targetPage route
    targetPage =
      fromMaybe
        (error $ "Can't find page with name '" ++ targetPageName ++ "', pointed to by route '" ++ Wasp.Route._urlPath route ++ "'")
        (find ((==) targetPageName . Wasp.Page._name) (Wasp.getPages wasp))

    determineRouteTargetComponent' :: Bool -> String
    determineRouteTargetComponent' authRequired =
      if authRequired
        then -- TODO(matija): would be nicer if this function name wasn't hardcoded here.
          "createAuthRequiredPage(" ++ targetPageName ++ ")"
        else targetPageName

createPageTemplateData :: Wasp.Page.Page -> PageTemplateData
createPageTemplateData page =
  PageTemplateData
    { _importFrom =
        relPathToExtSrcDir
          ++ SP.fromRelFileP (fromJust $ SP.relFileToPosix $ Wasp.JsImport._from pageComponent),
      _importWhat = case Wasp.JsImport._namedImports pageComponent of
        -- If no named imports, we go with the default import.
        [] -> pageName
        [namedImport] -> "{ " ++ namedImport ++ " as " ++ pageName ++ " }"
        _ -> error "Only one named import can be provided for a page."
    }
  where
    relPathToExtSrcDir :: FilePath
    relPathToExtSrcDir = "./ext-src/"

    pageName = Wasp.Page._name page
    pageComponent = Wasp.Page._component page
