{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator
  ( genWebApp,
    npmDepsForWasp,
  )
where

import Data.Aeson (object, (.=))
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified FilePath.Extra as FP.Extra
import StrongPath
  ( Dir,
    File',
    Path,
    Path',
    Posix,
    Rel,
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.Valid (getApp)
import Wasp.Env (envVarsToDotEnvContent)
import Wasp.Generator.Common
  ( makeJsArrayFromHaskellList,
  )
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)
import qualified Wasp.Generator.FileDraft as FD
import Wasp.Generator.JsImport (jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.WebAppGenerator.AuthG (genAuth)
import Wasp.Generator.WebAppGenerator.Common (webAppRootDirInProjectRootDir, webAppSrcDirInWebAppRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.WebAppGenerator.RouterGenerator (genRouter)
import qualified Wasp.Generator.WebSocket as AS.WS
import Wasp.JsImport
  ( JsImport,
    JsImportName (JsImportModule),
    JsImportPath (RelativeImportPath),
    makeJsImport,
  )
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import qualified Wasp.Project.Common as Project
import Wasp.Util ((<++>))

genWebApp :: AppSpec -> Generator [FileDraft]
genWebApp spec = do
  sequence
    [ genFileCopy [relfile|README.md|],
      genFileCopy [relfile|tsconfig.json|],
      genFileCopy [relfile|tsconfig.node.json|],
      genFileCopy [relfile|netlify.toml|],
      genPackageJson spec (npmDepsForWasp spec),
      genNpmrc,
      genGitignore,
      genIndexHtml spec,
      genViteConfig spec
    ]
    <++> genSrcDir spec
    <++> genPublicDir spec
    <++> genDotEnv spec
    <++> genEnvValidationScript
  where
    genFileCopy = return . C.mkTmplFd

genDotEnv :: AppSpec -> Generator [FileDraft]
-- Don't generate .env if we are building for production, since .env is to be used only for
-- development.
genDotEnv spec | AS.isBuild spec = return []
genDotEnv spec =
  return
    [ createTextFileDraft
        (C.webAppRootDirInProjectRootDir </> dotEnvInWebAppRootDir)
        (envVarsToDotEnvContent $ AS.devEnvVarsClient spec)
    ]

dotEnvInWebAppRootDir :: Path' (Rel C.WebAppRootDir) File'
dotEnvInWebAppRootDir = [relfile|.env|]

genPackageJson :: AppSpec -> N.NpmDepsForWasp -> Generator FileDraft
genPackageJson spec waspDependencies = do
  combinedDependencies <- N.genNpmDepsForPackage spec waspDependencies
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|package.json|])
      (C.asWebAppFile [relfile|package.json|])
      ( Just $
          object
            [ "appName" .= (fst (getApp spec) :: String),
              "depsChunk" .= N.getDependenciesPackageJsonEntry combinedDependencies,
              "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry combinedDependencies,
              "nodeVersionRange" .= (">=" <> show NodeVersion.oldestWaspSupportedNodeVersion)
            ]
      )

genNpmrc :: Generator FileDraft
genNpmrc =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|npmrc|])
      (C.asWebAppFile [relfile|.npmrc|])
      Nothing

npmDepsForWasp :: AppSpec -> N.NpmDepsForWasp
npmDepsForWasp _spec =
  N.NpmDepsForWasp
    { N.waspDependencies =
        AS.Dependency.fromList
          [ ("axios", "^1.4.0"),
            ("react", "^18.2.0"),
            ("react-dom", "^18.2.0"),
            ("@tanstack/react-query", "^4.29.0"),
            ("react-router-dom", "^5.3.3"),
            ("superjson", "^1.12.2"),
            ("mitt", "3.0.0"),
            -- Used for Auth UI
            ("react-hook-form", "^7.45.4")
          ],
      N.waspDevDependencies =
        AS.Dependency.fromList
          [ -- TODO: Allow users to choose whether they want to use TypeScript
            -- in their projects and install these dependencies accordingly.
            ("typescript", "^5.1.0"),
            ("@types/react", "^18.0.37"),
            ("@types/react-dom", "^18.0.11"),
            ("@types/react-router-dom", "^5.3.3"),
            ("@vitejs/plugin-react", "^4.2.1"),
            ("dotenv", "^16.0.3"),
            -- NOTE: Make sure to bump the version of the tsconfig
            -- when updating Vite or React versions
            ("@tsconfig/vite-react", "^2.0.0")
          ]
    }

genGitignore :: Generator FileDraft
genGitignore =
  return $
    C.mkTmplFdWithDst
      (C.asTmplFile [relfile|gitignore|])
      (C.asWebAppFile [relfile|.gitignore|])

genPublicDir :: AppSpec -> Generator [FileDraft]
genPublicDir spec =
  return $
    extPublicFileDrafts
      ++ ifUserDidntProvideFile genFaviconFd
      ++ ifUserDidntProvideFile genManifestFd
  where
    publicFiles = AS.externalPublicFiles spec
    extPublicFileDrafts = map C.mkPublicFileDraft publicFiles
    genFaviconFd = C.mkTmplFd (C.asTmplFile [relfile|public/favicon.ico|])
    genManifestFd = C.mkTmplFdWithData tmplFile tmplData
      where
        tmplData = object ["appName" .= (fst (getApp spec) :: String)]
        tmplFile = C.asTmplFile [relfile|public/manifest.json|]

    ifUserDidntProvideFile fileDraft = [fileDraft | not (checkIfFileDraftExists fileDraft)]
    checkIfFileDraftExists = (`elem` existingDstPaths) . FD.getDstPath
    existingDstPaths = map FD.getDstPath extPublicFileDrafts

genIndexHtml :: AppSpec -> Generator FileDraft
genIndexHtml spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|index.html|])
      targetPath
      (Just templateData)
  where
    targetPath = [relfile|index.html|]
    templateData =
      object
        [ "title" .= (AS.App.title (snd $ getApp spec) :: String),
          "head" .= (maybe "" (intercalate "\n") (AS.App.head $ snd $ getApp spec) :: String)
        ]

-- TODO(matija): Currently we also generate auth-specific parts in this file (e.g. token management),
-- although they are not used anywhere outside.
-- We could further "templatize" this file so only what is needed is generated.
genSrcDir :: AppSpec -> Generator [FileDraft]
genSrcDir spec =
  sequence
    [ genFileCopy [relfile|logo.png|],
      genFileCopy [relfile|utils.js|],
      genFileCopy [relfile|vite-env.d.ts|],
      genFileCopy [relfile|test/vitest/setup.ts|],
      genFileCopy [relfile|components/Message.tsx|],
      genFileCopy [relfile|components/Loader.tsx|],
      genFileCopy [relfile|components/FullPageWrapper.tsx|],
      getIndexTs spec
    ]
    <++> genAuth spec
    <++> genRouter spec
  where
    genFileCopy = return . C.mkSrcTmplFd

getIndexTs :: AppSpec -> Generator FileDraft
getIndexTs spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/index.tsx|])
      (C.asWebAppFile [relfile|src/index.tsx|])
      ( Just $
          object
            [ "setupFn" .= extImportToImportJson relPathToWebAppSrcDir maybeSetupJsFunction,
              "areWebSocketsUsed" .= AS.WS.areWebSocketsUsed spec
            ]
      )
  where
    maybeSetupJsFunction = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)

    relPathToWebAppSrcDir :: Path Posix (Rel importLocation) (Dir C.WebAppSrcDir)
    relPathToWebAppSrcDir = [reldirP|./|]

genEnvValidationScript :: Generator [FileDraft]
genEnvValidationScript =
  return
    [ C.mkTmplFd [relfile|scripts/validate-env.mjs|]
    ]

-- todo(filip): Take care of this as well
genViteConfig :: AppSpec -> Generator FileDraft
genViteConfig spec = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = C.asTmplFile [relfile|vite.config.ts|]
    tmplData =
      object
        [ "customViteConfig" .= jsImportToImportJson (makeCustomViteConfigJsImport <$> AS.customViteConfigPath spec),
          "baseDir" .= SP.fromAbsDirP (C.getBaseDir spec),
          "defaultClientPort" .= C.defaultClientPort,
          "vitest"
            .= object
              [ "setupFilesArray" .= makeJsArrayFromHaskellList vitestSetupFiles,
                "excludeWaspArtefactsPattern" .= (SP.fromRelDirP (fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir) FP.Posix.</> "**" FP.Posix.</> "*")
              ]
        ]
    vitestSetupFiles =
      [ SP.fromRelFile $
          dotWaspDirInWaspProjectDir
            </> generatedCodeDirInDotWaspDir
            </> webAppRootDirInProjectRootDir
            </> webAppSrcDirInWebAppRootDir
            </> [relfile|test/vitest/setup.ts|]
      ]

    makeCustomViteConfigJsImport :: Path' (Rel Project.WaspProjectDir) File' -> JsImport
    makeCustomViteConfigJsImport pathToConfig = makeJsImport (RelativeImportPath importPath) importName
      where
        importPath = SP.castRel $ C.toViteImportPath relPathToConfigInProjectDir
        relPathToConfigInProjectDir = relPathFromWebAppRootDirWaspProjectDir </> (fromJust . SP.relFileToPosix $ pathToConfig)

        relPathFromWebAppRootDirWaspProjectDir :: Path Posix (Rel C.WebAppRootDir) (Dir Project.WaspProjectDir)
        relPathFromWebAppRootDirWaspProjectDir =
          fromJust $
            SP.parseRelDirP $
              FP.Extra.reversePosixPath $
                SP.fromRelDir (Project.dotWaspDirInWaspProjectDir </> Project.generatedCodeDirInDotWaspDir </> C.webAppRootDirInProjectRootDir)

        importName = JsImportModule "customViteConfig"
