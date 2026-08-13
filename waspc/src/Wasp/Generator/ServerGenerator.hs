{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Wasp.Generator.ServerGenerator
  ( genServer,
    operationsRouteInRootRouter,
    npmDepsFromWasp,

    -- * Exported for testing only
    genDotEnv,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as ByteStringLazyUTF8
import Data.Maybe
  ( isJust,
    maybeToList,
  )
import StrongPath
  ( File,
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
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.Util as AS.Util
import Wasp.AppSpec.Valid (getApp, getLowestNodeVersionUserAllows, isAuthEnabled)
import Wasp.Env (envVarsToDotEnvContent)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.Generator.Crud.Routes as CrudRoutes
import Wasp.Generator.DepVersions
  ( expressTypesVersionRange,
    expressVersionRange,
    nitroVersion,
    superjsonVersionRange,
    typescriptVersionRange,
  )
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.NpmDependencies (NpmDepsForPackage (peerDependencies))
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.NpmWorkspaces (serverPackageName)
import Wasp.Generator.ServerGenerator.ApiRoutesG (genApis)
import Wasp.Generator.ServerGenerator.AuthG (genAuth)
import Wasp.Generator.ServerGenerator.Common (operationsRouteInRootRouter)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.CrudG (genCrud)
import Wasp.Generator.ServerGenerator.Db.Seed
  ( areDbSeedsDefined,
    dbSeedBundleFromServerRootDir,
    dbSeedViteConfigInServerRootDir,
    genDbSeed,
    getPackageJsonPrismaSeedField,
  )
import Wasp.Generator.ServerGenerator.JobGenerator (genJobs)
import Wasp.Generator.ServerGenerator.JsImport (getAliasedJsImportStmtAndIdentifier)
import Wasp.Generator.ServerGenerator.NitroRoutesG (genNitro)
import Wasp.Generator.ServerGenerator.OperationsG (genOperations)
import Wasp.Generator.ServerGenerator.OperationsRoutesG (genOperationsRoutes)
import Wasp.Generator.ServerGenerator.VirtualUserModulesPluginG (genVirtualUserModulesPlugin)
import Wasp.Generator.ServerGenerator.WebSocketG (genWebSockets)
import Wasp.Generator.WaspLibs.AvailableLibs (waspLibs)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import qualified Wasp.Generator.WebAppGenerator as WebApp
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (SrcTsConfigFile, waspProjectDirFromGeneratedAppComponentDir)
import Wasp.Project.Db (databaseUrlEnvVarName)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util ((<++>))

genServer :: AppSpec -> Generator [FileDraft]
genServer spec =
  sequence
    [ genFileCopy [relfile|README.md|],
      genVirtualUserModulesPlugin spec,
      genTsConfigJson spec,
      genPackageJson spec npmDeps,
      genGitignore
    ]
    <++> genNpmrc spec
    <++> genSrcDir spec
    <++> genDotEnv spec
    <++> genJobs spec
    <++> genApis spec
    <++> genCrud spec
  where
    genFileCopy = return . C.mkTmplFd
    npmDeps = npmDepsFromWasp spec

genDotEnv :: AppSpec -> Generator [FileDraft]
-- Don't generate .env if we are building for production, since .env is to be used only for
-- development.
genDotEnv spec | AS.isProduction spec = return []
genDotEnv spec =
  return
    [ createTextFileDraft
        (C.serverRootDirInGeneratedAppDir </> C.dotEnvInServerRootDir)
        (envVarsToDotEnvContent envVars)
    ]
  where
    envVars = waspEnvVars ++ userEnvVars
    userEnvVars = spec.devEnvVarsServer
    waspEnvVars = case spec.devDatabaseUrl of
      Just url | not isThereCustomDbUrl -> [(databaseUrlEnvVarName, url)]
      _ -> []
    isThereCustomDbUrl = any ((== databaseUrlEnvVarName) . fst) userEnvVars

genTsConfigJson :: AppSpec -> Generator FileDraft
genTsConfigJson spec = do
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|tsconfig.json|])
      (C.asServerFile [relfile|tsconfig.json|])
      ( Just $
          object
            [ "majorNodeVersion" .= show (SV.major NodeVersion.oldestWaspSupportedNodeVersion),
              "srcTsConfigPath" .= SP.fromRelFile srcTsConfigPath
            ]
      )
  where
    srcTsConfigPath :: Path' (Rel C.ServerRootDir) (File SrcTsConfigFile) =
      waspProjectDirFromGeneratedAppComponentDir </> AS.srcTsConfigPath spec

genPackageJson :: AppSpec -> N.NpmDepsFromWasp -> Generator FileDraft
genPackageJson spec waspDependencies =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|package.json|])
      (C.asServerFile [relfile|package.json|])
      ( Just $
          object
            [ "packageName" .= serverPackageName,
              "depsChunk" .= N.getDependenciesPackageJsonEntry serverDeps,
              "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry serverDeps,
              "nodeVersionRange" .= (">=" <> show NodeVersion.oldestWaspSupportedNodeVersion),
              "areDbSeedsDefined" .= areDbSeedsDefined spec,
              "dbSeedScript" .= dbSeedScript,
              "startProductionScript" .= startProductionScript,
              "prisma" .= ByteStringLazyUTF8.toString (Aeson.encode $ getPackageJsonPrismaField spec)
            ]
      )
  where
    serverDeps = N.mergeWaspAndUserDeps waspDependencies $ N.getUserNpmDepsForPackage spec

    hasEntities = AS.Util.hasEntities spec

    startProductionScript =
      (if hasEntities then "npm run db-migrate-prod && " else "")
        ++ unwords
          [ "NODE_ENV=production",
            -- Nitro's server reads `NITRO_PORT` before `PORT`, so a platform
            -- that sets `NITRO_PORT` itself would otherwise quietly win over
            -- the `PORT` Wasp's users (and Wasp's own deployments) set.
            "NITRO_PORT=${PORT:-" ++ show C.defaultServerPort ++ "}",
            "node",
            "--enable-source-maps",
            -- Wasp doesn't generate this file for production (the environment
            -- is a deployment's business), so this is only for the people
            -- running the built app themselves.
            "--env-file-if-exists=.env",
            SP.fromRelFileP nitroServerEntryFromServerRootDir
          ]

    dbSeedScript =
      unwords
        [ "vite build --config " ++ SP.fromRelFile dbSeedViteConfigInServerRootDir,
          "&& node --enable-source-maps --env-file-if-exists=.env",
          SP.fromRelFileP dbSeedBundleFromServerRootDir
        ]

-- | Where the app's production launcher (which runs from the generated server's
-- directory) finds the server Nitro builds.
nitroServerEntryFromServerRootDir :: Path Posix (Rel C.ServerRootDir) File'
nitroServerEntryFromServerRootDir =
  [reldirP|../|] </> WebApp.nitroServerEntryInGeneratedAppDir

getPackageJsonPrismaField :: AppSpec -> Aeson.Value
getPackageJsonPrismaField spec = object $ [] <> seedEntry
  where
    seedEntry = maybeToList $ Just . ("seed" .=) =<< getPackageJsonPrismaSeedField spec

npmDepsFromWasp :: AppSpec -> N.NpmDepsFromWasp
npmDepsFromWasp spec =
  N.NpmDepsFromWasp $
    N.NpmDepsForPackage
      { N.dependencies =
          Npm.Dependency.fromList
            [ ("cookie-parser", "~1.4.6"),
              ("cors", "^2.8.5"),
              ("express", show expressVersionRange),
              ("morgan", "~1.11.0"),
              ("helmet", "^6.0.0"),
              ("superjson", show superjsonVersionRange),
              -- Declared (not just hoisted from the SDK): `src/nitro/*.ts`
              -- imports `nitro/h3`, and the server's `tsc --build` compiles it.
              ("nitro", nitroVersion)
            ]
            ++ waspLibsNpmDeps,
        N.devDependencies =
          Npm.Dependency.fromList
            [ -- TODO: Allow users to choose whether they want to use TypeScript
              -- in their projects and install these dependencies accordingly.
              ("typescript", show typescriptVersionRange),
              ("@types/express", show expressTypesVersionRange),
              ("@types/express-serve-static-core", show expressTypesVersionRange),
              ("@types/node", show $ NodeVersion.nodeTypesVersionRangeMatchingNodeMajor $ getLowestNodeVersionUserAllows spec),
              ("@tsconfig/node" <> majorNodeVersionStr, "latest"),
              ("@types/cors", "^2.8.5")
            ],
        peerDependencies = []
      }
  where
    majorNodeVersionStr = show (SV.major $ getLowestNodeVersionUserAllows spec)

    waspLibsNpmDeps = map (WaspLib.makeLocalNpmDepFromWaspLib C.libsRootDirFromServerDir) waspLibs

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
            (C.asServerFile [relfile|.npmrc|])
            Nothing
        ]
  | otherwise =
      return []

genGitignore :: Generator FileDraft
genGitignore =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|gitignore|])
      (C.asServerFile [relfile|.gitignore|])
      Nothing

genSrcDir :: AppSpec -> Generator [FileDraft]
genSrcDir spec =
  sequence
    [ genFileCopy [relfile|app.js|]
    ]
    <++> genRoutesDir spec
    <++> genNitro spec
    <++> genOperationsRoutes spec
    <++> genOperations spec
    <++> genAuth spec
    <++> genDbSeed spec
    <++> genMiddleware spec
    <++> genWebSockets spec
  where
    genFileCopy = return . C.mkSrcTmplFd

genRoutesDir :: AppSpec -> Generator [FileDraft]
genRoutesDir spec =
  -- TODO(martin): We will probably want to extract "routes" path here same as we did with "src", to avoid hardcoding,
  -- but I did not bother with it yet since it is used only here for now.
  sequence [genRoutesIndex spec]

genRoutesIndex :: AppSpec -> Generator FileDraft
genRoutesIndex spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/routes/index.js|])
      (C.asServerFile [relfile|src/routes/index.js|])
      (Just tmplData)
  where
    tmplData =
      object
        [ "operationsRouteInRootRouter" .= (operationsRouteInRootRouter :: String),
          "crudRouteInRootRouter" .= (CrudRoutes.crudRouteInRootRouter :: String),
          "healthRoutePath" .= (C.healthRoutePath :: String),
          "isAuthEnabled" .= (isAuthEnabled spec :: Bool),
          "areThereAnyCustomApiRoutes" .= (not . null $ AS.getApis spec),
          "areThereAnyCrudRoutes" .= (not . null $ AS.getCruds spec)
        ]

genMiddleware :: AppSpec -> Generator [FileDraft]
genMiddleware spec =
  sequence
    [ return $ C.mkTmplFd [relfile|src/middleware/index.ts|],
      return $ C.mkTmplFdWithData [relfile|src/middleware/globalMiddleware.ts|] (Just tmplData),
      genOperationsMiddleware spec
    ]
  where
    tmplData =
      object
        [ "globalMiddlewareConfigFn" .= globalMiddlewareConfigFnTmplData
        ]

    globalMiddlewareConfigFnTmplData :: Aeson.Value
    globalMiddlewareConfigFnTmplData =
      let maybeGlobalMiddlewareConfigFn = AS.App.server (snd $ getApp spec) >>= AS.App.Server.middlewareConfigFn
          globalMiddlewareConfigFnAlias = "_waspGlobalMiddlewareConfigFn"
          maybeGlobalMidlewareConfigFnImports = getAliasedJsImportStmtAndIdentifier globalMiddlewareConfigFnAlias [reldirP|../|] <$> maybeGlobalMiddlewareConfigFn
       in object
            [ "isDefined" .= isJust maybeGlobalMidlewareConfigFnImports,
              "importStatement" .= maybe "" fst maybeGlobalMidlewareConfigFnImports,
              "importAlias" .= globalMiddlewareConfigFnAlias
            ]

genOperationsMiddleware :: AppSpec -> Generator FileDraft
genOperationsMiddleware spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/middleware/operations.ts|])
      (C.asServerFile [relfile|src/middleware/operations.ts|])
      (Just tmplData)
  where
    tmplData = object ["isAuthEnabled" .= (isAuthEnabled spec :: Bool)]
