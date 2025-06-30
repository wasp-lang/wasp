{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator
  ( genWebApp,
    npmDepsForWasp,
  )
where

import Data.Aeson (object, (.=))
import Data.List (intercalate)
import StrongPath
  ( Dir,
    File,
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
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.Valid (getApp)
import Wasp.Env (envVarsToDotEnvContent)
import Wasp.Generator.Common (typescriptVersion)
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)
import qualified Wasp.Generator.FileDraft as FD
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.WebAppGenerator.AuthG (genAuth)
import Wasp.Generator.WebAppGenerator.Common
  ( axiosVersion,
    reactQueryVersion,
    reactRouterVersion,
    reactVersion,
  )
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.WebAppGenerator.RouterGenerator (genRouter)
import Wasp.Generator.WebAppGenerator.Vite (genVite)
import qualified Wasp.Generator.WebSocket as AS.WS
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common
  ( SrcTsConfigFile,
    waspProjectDirFromAppComponentDir,
  )
import Wasp.Util ((<++>))

genWebApp :: AppSpec -> Generator [FileDraft]
genWebApp spec = do
  sequence
    [ genFileCopy [relfile|README.md|],
      genFileCopy [relfile|tsconfig.json|],
      genAppTsConfigJson spec,
      genFileCopy [relfile|netlify.toml|],
      genPackageJson spec (npmDepsForWasp spec),
      genNpmrc,
      genGitignore,
      genIndexHtml spec
    ]
    <++> genSrcDir spec
    <++> genPublicDir spec
    <++> genDotEnv spec
    <++> genVite spec
  where
    genFileCopy = return . C.mkTmplFd

genAppTsConfigJson :: AppSpec -> Generator FileDraft
genAppTsConfigJson spec = do
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|tsconfig.app.json|])
      (C.asWebAppFile [relfile|tsconfig.app.json|])
      ( Just $
          object
            [ "srcTsConfigPath" .= SP.fromRelFile srcTsConfigPath
            ]
      )
  where
    srcTsConfigPath :: Path' (Rel C.WebAppRootDir) (File SrcTsConfigFile) =
      waspProjectDirFromAppComponentDir </> AS.srcTsConfigPath spec

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
          [ ("axios", show axiosVersion),
            ("react", show reactVersion),
            -- React and ReactDOM versions should always match.
            ("react-dom", show reactVersion),
            ("@tanstack/react-query", show reactQueryVersion),
            ("react-router-dom", show reactRouterVersion)
          ],
      N.waspDevDependencies =
        AS.Dependency.fromList
          [ -- TODO: Allow users to choose whether they want to use TypeScript
            -- in their projects and install these dependencies accordingly.
            ("typescript", show typescriptVersion),
            ("@types/react", "^18.0.37"),
            ("@types/react-dom", "^18.0.11"),
            -- @vitejs/plugin-react is pinned down to an older version to
            -- prevent NPM from installing Vite 7 as its transient dependency (this
            -- breaks Wasp with Node older than 20.19)
            -- TODO: Remove after figuring out our dependency story. Relevant issues:
            -- - todo: add
            -- - todo: add
            ("@vitejs/plugin-react", "4.5.1"),
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
      genFileCopy [relfile|components/Loader.module.css|],
      genFileCopy [relfile|components/FullPageWrapper.tsx|],
      genFileCopy [relfile|components/DefaultRootErrorBoundary.tsx|],
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
