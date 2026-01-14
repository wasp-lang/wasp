{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator
  ( genWebApp,
    npmDepsFromWasp,
  )
where

import Data.Aeson (object, (.=))
import StrongPath
  ( File,
    File',
    Path',
    Rel,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.Valid (getApp)
import Wasp.Env (envVarsToDotEnvContent)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.DepVersions
  ( axiosVersion,
    reactDomTypesVersion,
    reactDomVersion,
    reactQueryVersion,
    reactRouterVersion,
    reactTypesVersion,
    reactVersion,
    typescriptVersion,
  )
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)
import qualified Wasp.Generator.FileDraft as FD
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.NpmWorkspaces (webAppPackageName)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.Vite (genVite)
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
      genPackageJson spec (npmDepsFromWasp spec),
      genGitignore
    ]
    <++> genNpmrc spec
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
genDotEnv spec | AS.isProduction spec = return []
genDotEnv spec =
  return
    [ createTextFileDraft
        (C.webAppRootDirInProjectRootDir </> dotEnvInWebAppRootDir)
        (envVarsToDotEnvContent $ AS.devEnvVarsClient spec)
    ]

dotEnvInWebAppRootDir :: Path' (Rel C.WebAppRootDir) File'
dotEnvInWebAppRootDir = [relfile|.env|]

genPackageJson :: AppSpec -> N.NpmDepsFromWasp -> Generator FileDraft
genPackageJson spec waspDependencies = do
  webAppDeps <- N.ensureNoConflictWithUserDeps waspDependencies $ N.getUserNpmDepsForPackage spec
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|package.json|])
      (C.asWebAppFile [relfile|package.json|])
      ( Just $
          object
            [ "packageName" .= webAppPackageName spec,
              "depsChunk" .= N.getDependenciesPackageJsonEntry webAppDeps,
              "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry webAppDeps,
              "overridesChunk" .= N.getDependencyOverridesPackageJsonEntry dependencyOverrides,
              "nodeVersionRange" .= (">=" <> show NodeVersion.oldestWaspSupportedNodeVersion)
            ]
      )
  where
    dependencyOverrides =
      Npm.Dependency.fromList
        [ -- TODO: remove this once Rollup fixes their lastest version https://github.com/rollup/rollup/issues/6012
          ("rollup", "4.44.0")
        ]

genNpmrc :: AppSpec -> Generator [FileDraft]
genNpmrc spec
  -- We only use `.npmrc` to force `npm` to error out if the Node.js version is incompatible.
  --
  -- In dev mode, we already check the Node.js version ourselves before running any `npm` commands,
  -- so we don't need this there.
  --
  -- We do expect users to manually go into the generated directories when bundling the built ouput.
  -- So we do add the `.npmrc` there to help them avoid using an incompatible Node.js version.
  | AS.isProduction spec =
      return
        [ C.mkTmplFdWithDstAndData
            (C.asTmplFile [relfile|npmrc|])
            (C.asWebAppFile [relfile|.npmrc|])
            Nothing
        ]
  | otherwise = return []

npmDepsFromWasp :: AppSpec -> N.NpmDepsFromWasp
npmDepsFromWasp _spec =
  N.NpmDepsFromWasp $
    N.NpmDepsForPackage
      { N.dependencies =
          Npm.Dependency.fromList
            [ ("axios", show axiosVersion),
              ("react", show reactVersion),
              ("react-dom", show reactDomVersion),
              ("@tanstack/react-query", reactQueryVersion),
              ("react-router-dom", show reactRouterVersion)
            ],
        N.devDependencies =
          Npm.Dependency.fromList
            [ -- TODO: Allow users to choose whether they want to use TypeScript
              -- in their projects and install these dependencies accordingly.
              ("typescript", show typescriptVersion),
              ("@types/react", show reactTypesVersion),
              ("@types/react-dom", show reactDomTypesVersion),
              ("@vitejs/plugin-react", "^4.7.0"),
              -- NOTE: Make sure to bump the version of the tsconfig
              -- when updating Vite or React versions
              ("@tsconfig/vite-react", "^7.0.0")
            ],
        N.peerDependencies = []
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
      ++ ifUserDidntProvideFile manifestFd
  where
    publicFiles = AS.externalPublicFiles spec
    extPublicFileDrafts = map C.mkPublicFileDraft publicFiles
    manifestFd = C.mkTmplFdWithData tmplFile tmplData
      where
        tmplData = object ["appName" .= (fst (getApp spec) :: String)]
        tmplFile = C.asTmplFile [relfile|public/manifest.json|]

    ifUserDidntProvideFile fileDraft = [fileDraft | not (checkIfFileDraftExists fileDraft)]
    checkIfFileDraftExists = (`elem` existingDstPaths) . FD.getDstPath
    existingDstPaths = map FD.getDstPath extPublicFileDrafts

genSrcDir :: AppSpec -> Generator [FileDraft]
genSrcDir spec =
  sequence
    [ genFileCopy [relfile|vite-env.d.ts|],
      genFileCopy [relfile|test/vitest/setup.ts|]
    ]
  where
    genFileCopy = return . C.mkSrcTmplFd
