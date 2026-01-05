module Wasp.Generator.SdkGenerator.Vite.VitePlugin
  ( genVitePlugins,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust, isJust)
import StrongPath (Dir, Path', Rel, reldir, relfile, (</>))
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
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.JsImport (JsImport (..), JsImportPath (..), applyJsImportAlias, getJsImportStmtAndIdentifier)
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir, srcDirInWaspProjectDir, waspProjectDirFromAppComponentDir)

genVitePlugins :: AppSpec -> Generator [FileDraft]
genVitePlugins spec =
  return $
    -- Root level files
    [ genWaspPlugin spec,
      genViteIndexPlugin
    ]
      ++
      -- Plugins directory files
      [ genDetectServerImportsPlugin,
        genValidateEnvPlugin,
        genWaspBuildConfigPlugin,
        genWaspVirtualModulesPlugin,
        genWaspHtmlPlugin
      ]

genWaspPlugin :: AppSpec -> FileDraft
genWaspPlugin spec = SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = SDK.asTmplFile $ viteDir </> [relfile|wasp.ts|]
    tmplData =
      object
        [ "baseDir" .= SP.fromAbsDirP (C.getBaseDir spec),
          "projectDir" .= SP.fromRelDir [reldir|.|],
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

genViteIndexPlugin :: FileDraft
genViteIndexPlugin = SDK.mkTmplFd $ SDK.asTmplFile $ viteDir </> [relfile|index.ts|]

genDetectServerImportsPlugin :: FileDraft
genDetectServerImportsPlugin = SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = SDK.asTmplFile $ pluginsDir </> [relfile|detectServerImports.ts|]
    tmplData =
      object
        [ "waspProjectDirFromWebAppDir" .= SP.fromRelDir waspProjectDirFromWebAppDir,
          "srcDirInWaspProjectDir" .= SP.fromRelDir srcDirInWaspProjectDir
        ]
    waspProjectDirFromWebAppDir = waspProjectDirFromAppComponentDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir)

genValidateEnvPlugin :: FileDraft
genValidateEnvPlugin = SDK.mkTmplFd $ SDK.asTmplFile $ pluginsDir </> [relfile|validateEnv.ts|]

genWaspBuildConfigPlugin :: FileDraft
genWaspBuildConfigPlugin = SDK.mkTmplFd $ SDK.asTmplFile $ pluginsDir </> [relfile|waspBuildConfig.ts|]

genWaspVirtualModulesPlugin :: FileDraft
genWaspVirtualModulesPlugin = SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = SDK.asTmplFile $ pluginsDir </> [relfile|waspVirtualModules.ts|]
    tmplData =
      object
        [ "indexVirtualFileName" .= ("/index.virtual.tsx" :: String),
          "routesVirtualFileName" .= ("./routes.virtual.tsx" :: String)
        ]

genWaspHtmlPlugin :: FileDraft
genWaspHtmlPlugin = SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = SDK.asTmplFile $ pluginsDir </> [relfile|waspHtml.ts|]
    tmplData =
      object
        [ "indexVirtualFileName" .= ("/index.virtual.tsx" :: String)
        ]

viteDir :: Path' (Rel SDK.SdkTemplatesDir) (Dir ViteDir)
viteDir = [reldir|client/vite|]

pluginsDir :: Path' (Rel SDK.SdkTemplatesDir) (Dir PluginsDir)
pluginsDir = viteDir </> [reldir|plugins|]

data ViteDir

data PluginsDir

getAppComponentImport :: Maybe AS.ExtImport.ExtImport -> String
getAppComponentImport Nothing = ""
getAppComponentImport (Just extImport) =
  let jsImport = extImportToSdkSrcRelativeImport extImport "App"
      (importStmt, _) = getJsImportStmtAndIdentifier $ applyJsImportAlias (Just "App") jsImport
   in importStmt

getClientSetupImport :: Maybe AS.ExtImport.ExtImport -> String
getClientSetupImport Nothing = ""
getClientSetupImport (Just extImport) =
  let jsImport = extImportToSdkSrcRelativeImport extImport "setup"
      (importStmt, _) = getJsImportStmtAndIdentifier $ applyJsImportAlias (Just "setup") jsImport
   in importStmt

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
