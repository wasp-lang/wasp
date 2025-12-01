module Wasp.Generator.UserDirGenerator
  ( genUserDir,
    genUserDirRoutes,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import StrongPath (Dir, Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.Generator.Common (ProjectRootDir, dropExtensionFromImportPath)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Project.Common (WaspProjectDir, waspProjectDirFromProjectRootDir)

-- | Phantom type for user-dir templates directory
data UserDirTemplatesDir

userDirTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir UserDirTemplatesDir)
userDirTemplatesDirInTemplatesDir = [reldir|user-dir|]

-- | Path to user's src directory relative to Wasp project directory
userSrcDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir ())
userSrcDirInWaspProjectDir = [reldir|src|]

-- | Generate all files for the user directory
genUserDir :: AppSpec -> Generator [FileDraft]
genUserDir spec = do
  routesFile <- genUserDirRoutes spec
  return [routesFile]

-- | Generate routes.generated.tsx in user's src directory
genUserDirRoutes :: AppSpec -> Generator FileDraft
genUserDirRoutes spec = do
  return $
    createTemplateFileDraft
      (waspProjectDirFromProjectRootDir </> userSrcDirInWaspProjectDir </> [relfile|routes.generated.tsx|])
      (userDirTemplatesDirInTemplatesDir </> [relfile|src/routes.generated.tsx|])
      (Just tmplData)
  where
    tmplData =
      object
        [ "routes" .= map (createRouteTemplateData spec) (AS.getRoutes spec),
          "pages" .= map createPageTemplateData (AS.getPages spec)
        ]

data RouteTemplateData = RouteTemplateData
  { _routeName :: !String,
    _targetComponent :: !String
  }

instance Aeson.ToJSON RouteTemplateData where
  toJSON routeTD =
    object
      [ "name" .= _routeName routeTD,
        "targetComponent" .= _targetComponent routeTD
      ]

data PageTemplateData = PageTemplateData
  { _importStmt :: !String
  }
  deriving (Show, Eq)

instance Aeson.ToJSON PageTemplateData where
  toJSON pageTD =
    object
      [ "importStatement" .= _importStmt pageTD
      ]

createRouteTemplateData :: AppSpec -> (String, AS.Route.Route) -> RouteTemplateData
createRouteTemplateData spec (name, route) =
  RouteTemplateData
    { _routeName = name,
      _targetComponent = determineRouteTargetComponent spec (name, route)
    }

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

createPageTemplateData :: (String, AS.Page.Page) -> PageTemplateData
createPageTemplateData page =
  PageTemplateData
    { _importStmt = importStmt
    }
  where
    importStmt :: String
    importStmt = case AS.ExtImport.name pageComponent of
      AS.ExtImport.ExtImportModule _ -> "import " ++ importAlias ++ " from './" ++ importPath ++ "'"
      AS.ExtImport.ExtImportField name -> "import { " ++ name ++ " as " ++ importAlias ++ " } from './" ++ importPath ++ "'"

    importPath :: String
    importPath = SP.fromRelFileP $ dropExtensionFromImportPath $ AS.ExtImport.path pageComponent

    pageComponent :: AS.ExtImport.ExtImport
    pageComponent = AS.Page.component $ snd page

    importAlias :: String
    importAlias = fst page
