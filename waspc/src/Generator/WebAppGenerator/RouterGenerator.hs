module Generator.WebAppGenerator.RouterGenerator
    ( generateRouter
    ) where

import           Data.Aeson                       (ToJSON (..), object, (.=))
import           Data.Maybe                       (isJust)
import qualified Path                             as P

import           Generator.FileDraft              (FileDraft)
import           Generator.WebAppGenerator.Common (asTmplFile, asWebAppSrcFile)
import qualified Generator.WebAppGenerator.Common as C
import           StrongPath                       ((</>))
import qualified StrongPath                       as SP
import           Wasp                             (Wasp)
import qualified Wasp
import qualified Wasp.JsImport
import qualified Wasp.Page
import qualified Wasp.Route


data RouterTemplateData = RouterTemplateData
    { _routes         :: ![RouteTemplateData]
    , _pagesToImport  :: ![PageTemplateData]
    , _isAuthEnabled  :: Bool
    }

instance ToJSON RouterTemplateData where
    toJSON routerTD = object
        [ "routes" .= _routes routerTD
        , "pagesToImport" .= _pagesToImport routerTD
        , "isAuthEnabled" .= _isAuthEnabled routerTD
        ]

data RouteTemplateData = RouteTemplateData
    { _urlPath         :: !String
    , _targetComponent :: !String
    }

instance ToJSON RouteTemplateData where
    toJSON routeTD = object
        [ "urlPath" .= _urlPath routeTD
        , "targetComponent" .= _targetComponent routeTD
        ]

data PageTemplateData = PageTemplateData
    { _importWhat :: !String
    , _importFrom :: !String
    } deriving (Show, Eq)

instance ToJSON PageTemplateData where
    toJSON pageTD = object
        [ "importWhat" .= _importWhat pageTD
        , "importFrom" .= _importFrom pageTD
        ]

generateRouter :: Wasp -> FileDraft
generateRouter wasp = C.makeTemplateFD
    (asTmplFile $ [P.reldir|src|] P.</> routerPath)
    targetPath
    (Just $ toJSON templateData)
    where
        routerPath = [P.relfile|router.js|]
        templateData = createRouterTemplateData wasp
        targetPath = C.webAppSrcDirInWebAppRootDir </> asWebAppSrcFile routerPath

createRouterTemplateData :: Wasp -> RouterTemplateData
createRouterTemplateData wasp = RouterTemplateData
    { _routes = routes
    , _pagesToImport = pages
    , _isAuthEnabled = isJust $ Wasp.getAuth wasp
    }
    where
        routes = map (createRouteTemplateData wasp) $ Wasp.getRoutes wasp
        pages = map createPageTemplateData $ Wasp.getPages wasp

createRouteTemplateData :: Wasp -> Wasp.Route.Route -> RouteTemplateData
createRouteTemplateData wasp route = RouteTemplateData
    { _urlPath = Wasp.Route._urlPath route
    , _targetComponent = determineRouteTargetComponent wasp route
    }

determineRouteTargetComponent :: Wasp -> Wasp.Route.Route -> String
determineRouteTargetComponent wasp route =
    maybe
        targetPageName
        determineRouteTargetComponent'
        (Wasp.Page._authRequired targetPage)
    where
        targetPageName = Wasp.Route._targetPage route
        -- NOTE(matija): if no page with the name specified in the route, head will fail.
        targetPage = head $ filter ((==) targetPageName . Wasp.Page._name) (Wasp.getPages wasp)

        -- | Applied if authRequired property is present.
        determineRouteTargetComponent' :: Bool -> String
        determineRouteTargetComponent' authRequired =
            if authRequired
            -- TODO(matija): would be nicer if this function name wasn't hardcoded here.
            then "createAuthRequiredPage(" ++ targetPageName ++ ")"
            else targetPageName



createPageTemplateData :: Wasp.Page.Page -> PageTemplateData
createPageTemplateData page = PageTemplateData
    { _importFrom = relPathToExtSrcDir ++
                    SP.toFilePath (SP.relFileToPosix' $ Wasp.JsImport._from pageComponent)
    , _importWhat = case Wasp.JsImport._namedImports pageComponent of
                        -- If no named imports, we go with the default import.
                        []              -> pageName
                        [namedImport]   -> "{ " ++ namedImport ++ " as " ++ pageName ++ " }"
                        _               -> error "Only one named import can be provided for a page."
    }
    where
        relPathToExtSrcDir :: FilePath
        relPathToExtSrcDir = "./ext-src/"

        pageName = Wasp.Page._name page
        pageComponent = Wasp.Page._component page
