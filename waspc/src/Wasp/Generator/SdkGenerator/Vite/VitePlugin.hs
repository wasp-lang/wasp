module Wasp.Generator.SdkGenerator.Vite.VitePlugin
  ( genVitePlugins,
    vitePlugins,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust, isJust)
import StrongPath (Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (WebAppRootDir, dropExtensionFromImportPath)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as SDK
import Wasp.Generator.SdkGenerator.Vite.VirtualFiles (indexTsxFileName, routesFileName)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.JsImport (JsImport (..), JsImportPath (..), applyJsImportAlias, getJsImportStmtAndIdentifier)
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir, srcDirInWaspProjectDir, waspProjectDirFromAppComponentDir)

data VitePluginName = DetectServerImports | ValidateEnv | VirtualFiles | Wasp | ViteIndex
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
    pluginFilePathInPluginsDir VirtualFiles = [relfile|virtualFiles.ts|]
    pluginFilePathInPluginsDir Wasp = [relfile|wasp.ts|]
    pluginFilePathInPluginsDir ViteIndex = [relfile|index.ts|]

genVitePlugin :: AppSpec -> VitePlugin -> Generator FileDraft
genVitePlugin _ (DetectServerImports, tmplFile) = genDetectServerImportsPlugin tmplFile
genVitePlugin _ (ValidateEnv, tmplFile) = genValidateEnvPlugin tmplFile
genVitePlugin spec (VirtualFiles, tmplFile) = genVirtualFilesPlugin spec tmplFile
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

genVirtualFilesPlugin :: AppSpec -> Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genVirtualFilesPlugin _spec tmplFile = return $ SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplData =
      object
        [ "indexTsxFileName" .= indexTsxFileName,
          "routesFileName" .= routesFileName
        ]

genViteIndexPlugin :: Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genViteIndexPlugin tmplFile = return $ SDK.mkTmplFd tmplFile

genWaspPlugin :: AppSpec -> Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genWaspPlugin spec tmplFile = do
  return $ SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplData =
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
