{-# LANGUAGE ImportQualifiedPost #-}

module Wasp.Generator.SdkGenerator.Vite.VitePlugin
  ( genVitePlugins,
    vitePlugins,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust, isJust)
import StrongPath (Dir, File', Path', Rel, reldir, relfile, (</>))
import StrongPath qualified as SP
import System.FilePath.Posix qualified as FP.Posix
import Wasp.AppSpec (AppSpec)
import Wasp.AppSpec qualified as AS
import Wasp.AppSpec.App qualified as AS.App
import Wasp.AppSpec.App.Client qualified as AS.App.Client
import Wasp.AppSpec.ExtImport qualified as AS.ExtImport
import Wasp.AppSpec.Page qualified as AS.Page
import Wasp.AppSpec.Route qualified as AS.Route
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.Common (WebAppRootDir, dropExtensionFromImportPath)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport qualified as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common qualified as SDK
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.WebAppGenerator.Common qualified as C
import Wasp.JsImport (JsImport (..), JsImportName (..), JsImportPath (..), applyJsImportAlias, getJsImportStmtAndIdentifier)
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir, srcDirInWaspProjectDir, waspProjectDirFromAppComponentDir)

data VitePluginName = DetectServerImports | ValidateEnv | Wasp | ViteIndex
  deriving (Enum, Bounded)

type TmplFilePath = Path' (Rel SDK.SdkTemplatesDir) File'

-- We define it like this because we need a list of plugin
-- paths which we will use in the tsconfig.vite.json "include" section
type VitePlugin = (VitePluginName, TmplFilePath)

genVitePlugins :: AppSpec -> Generator [FileDraft]
genVitePlugins spec = mapM (genVitePlugin spec) vitePlugins

vitePlugins :: [VitePlugin]
vitePlugins =
  map
    (\name -> (name, getTmplFilePathForVitePlugin name))
    vitePluginNames
  where
    vitePluginNames = [minBound .. maxBound]

getTmplFilePathForVitePlugin :: VitePluginName -> TmplFilePath
getTmplFilePathForVitePlugin pluginName = SDK.asTmplFile $ [reldir|client/vite|] </> pluginFilePathInPluginsDir pluginName
  where
    pluginFilePathInPluginsDir DetectServerImports = [relfile|detectServerImports.ts|]
    pluginFilePathInPluginsDir ValidateEnv = [relfile|validateEnv.ts|]
    pluginFilePathInPluginsDir Wasp = [relfile|wasp.ts|]
    pluginFilePathInPluginsDir ViteIndex = [relfile|index.ts|]

genVitePlugin :: AppSpec -> VitePlugin -> Generator FileDraft
genVitePlugin _ (DetectServerImports, tmplFile) = genDetectServerImportsPlugin tmplFile
genVitePlugin _ (ValidateEnv, tmplFile) = genValidateEnvPlugin tmplFile
genVitePlugin spec (Wasp, tmplFile) = genWaspPlugin spec tmplFile
genVitePlugin _ (ViteIndex, tmplFile) = genViteIndexPlugin tmplFile

genDetectServerImportsPlugin :: Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genDetectServerImportsPlugin tmplFile = return $ SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplData =
      object
        [ "waspProjectDirFromWebAppDir" .= SP.fromRelDir waspProjectDirFromWebAppDir,
          "srcDirInWaspProjectDir" .= SP.fromRelDir srcDirInWaspProjectDir
        ]

    waspProjectDirFromWebAppDir = waspProjectDirFromAppComponentDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir)

genValidateEnvPlugin :: Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genValidateEnvPlugin tmplFile = return $ SDK.mkTmplFd tmplFile

genViteIndexPlugin :: Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genViteIndexPlugin tmplFile = return $ SDK.mkTmplFd tmplFile

genWaspPlugin :: AppSpec -> Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genWaspPlugin spec tmplFile = do
  let routesContent = genRoutesContent spec
  return $ SDK.mkTmplFdWithData tmplFile $ tmplData routesContent
  where
    tmplData routesContent =
      object
        [ "baseDir" .= SP.fromAbsDirP (C.getBaseDir spec),
          "projectDir" .= (SP.fromRelDir $ [reldir|.|]),
          "defaultClientPort" .= C.defaultClientPort,
          "buildOutputDir" .= (".wasp/out/web-app/build" :: String),
          "htmlTitle" .= AS.App.title (snd $ getApp spec),
          "hasAppComponent" .= isJust maybeRootComponent,
          "appComponentImport" .= getAppComponentImport maybeRootComponent,
          "hasClientSetup" .= isJust maybeSetupFn,
          "clientSetupImport" .= getClientSetupImport maybeSetupFn,
          "routesContent" .= routesContent,
          "vitest"
            .= object
              [ "excludeWaspArtefactsPattern" .= (SP.fromRelDirP (fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir) FP.Posix.</> "**" FP.Posix.</> "*")
              ]
        ]

    maybeRootComponent = AS.App.Client.rootComponent =<< AS.App.client (snd $ getApp spec)
    maybeSetupFn = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)

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

-- | Generate the routes.generated.tsx content as a string
genRoutesContent :: AppSpec -> String
genRoutesContent spec =
  unlines $
    [ "// This file is auto-generated by Wasp.",
      "// DO NOT EDIT THIS FILE MANUALLY",
      ""
    ]
      ++ ( if isAuthEnabled spec
             then
               [ "import { createAuthRequiredPage } from \"wasp/client/auth\";",
                 ""
               ]
             else []
         )
      ++ concat (map (lines . createPageImportStatement) (AS.getPages spec))
      ++ [ "",
           "export const routes = {"
         ]
      ++ concat (map (\r -> ["  " ++ createRouteExport spec r ++ ","]) (AS.getRoutes spec))
      ++ [ "} as const;"
         ]

createPageImportStatement :: (String, AS.Page.Page) -> String
createPageImportStatement (pageName, page) =
  case AS.ExtImport.name pageComponent of
    AS.ExtImport.ExtImportModule _ -> "import " ++ pageName ++ " from \"./src/" ++ importPath ++ "\";"
    AS.ExtImport.ExtImportField name -> "import { " ++ name ++ " as " ++ pageName ++ " } from \"./src/" ++ importPath ++ "\";"
  where
    importPath = SP.fromRelFileP $ dropExtensionFromImportPath $ AS.ExtImport.path pageComponent
    pageComponent = AS.Page.component page

createRouteExport :: AppSpec -> (String, AS.Route.Route) -> String
createRouteExport spec (name, route) =
  name ++ ": " ++ determineRouteTargetComponent spec (name, route)

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
