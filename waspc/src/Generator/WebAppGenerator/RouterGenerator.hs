module Generator.WebAppGenerator.RouterGenerator
    ( generateRouter
    ) where

import           Data.Aeson                       (ToJSON (..), object, (.=))
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
    { _routes        :: ![Wasp.Route.Route]
    , _pagesToImport :: ![PageTemplateData]
    }

instance ToJSON RouterTemplateData where
    toJSON routerTD = object
        [ "routes" .= _routes routerTD
        , "pagesToImport" .= _pagesToImport routerTD
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
        targetPath = C.webAppSrcDirInWebAppRootDir </> (asWebAppSrcFile routerPath)

createRouterTemplateData :: Wasp -> RouterTemplateData
createRouterTemplateData wasp = RouterTemplateData
    { _routes = routes
    , _pagesToImport = pages
    }
    where
        routes = Wasp.getRoutes wasp
        pages = map createPageTemplateData $ Wasp.getPages wasp


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
