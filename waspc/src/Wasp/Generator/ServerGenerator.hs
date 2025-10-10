{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Wasp.Generator.ServerGenerator
  ( genServer,
    operationsRouteInRootRouter,
    npmDepsFromWasp,
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
  ( Dir,
    File,
    File',
    Path,
    Path',
    Posix,
    Rel,
    fromRelDir,
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import qualified Wasp.AppSpec.Util as AS.Util
import Wasp.AppSpec.Valid (getApp, getLowestNodeVersionUserAllows, isAuthEnabled)
import Wasp.Env (envVarsToDotEnvContent)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Common (ServerRootDir)
import qualified Wasp.Generator.Crud.Routes as CrudRoutes
import Wasp.Generator.DepVersions (superjsonVersion, typescriptVersion)
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.NpmDependencies (NpmDepsForPackage (peerDependencies))
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.NpmWorkspaces (serverPackageName)
import Wasp.Generator.ServerGenerator.ApiRoutesG (genApis)
import Wasp.Generator.ServerGenerator.AuthG (genAuth)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.CrudG (genCrud)
import Wasp.Generator.ServerGenerator.Db.Seed (genDbSeed, getDbSeeds, getPackageJsonPrismaSeedField)
import Wasp.Generator.ServerGenerator.DepVersions (expressTypesVersion, expressVersionStr)
import Wasp.Generator.ServerGenerator.JobGenerator (genJobs)
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson, getAliasedJsImportStmtAndIdentifier)
import Wasp.Generator.ServerGenerator.OperationsG (genOperations)
import Wasp.Generator.ServerGenerator.OperationsRoutesG (genOperationsRoutes)
import Wasp.Generator.ServerGenerator.WebSocketG (depsRequiredByWebSockets, genWebSockets, mkWebSocketFnImport)
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (SrcTsConfigFile, srcDirInWaspProjectDir, waspProjectDirFromAppComponentDir)
import Wasp.Project.Db (databaseUrlEnvVarName)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util ((<++>))

genServer :: AppSpec -> Generator [FileDraft]
genServer spec =
  sequence
    [ genFileCopy [relfile|README.md|],
      genRollupConfigJs spec,
      genTsConfigJson spec,
      genPackageJson spec (npmDepsFromWasp spec),
      genGitignore,
      genNodemon
    ]
    <++> genNpmrc spec
    <++> genSrcDir spec
    <++> genDotEnv spec
    <++> genJobs spec
    <++> genApis spec
    <++> genCrud spec
  where
    genFileCopy = return . C.mkTmplFd

genDotEnv :: AppSpec -> Generator [FileDraft]
-- Don't generate .env if we are building for production, since .env is to be used only for
-- development.
genDotEnv spec | AS.isBuild spec = return []
genDotEnv spec =
  return
    [ createTextFileDraft
        (C.serverRootDirInProjectRootDir </> dotEnvInServerRootDir)
        (envVarsToDotEnvContent envVars)
    ]
  where
    envVars = waspEnvVars ++ userEnvVars
    userEnvVars = AS.devEnvVarsServer spec
    waspEnvVars = case AS.devDatabaseUrl spec of
      Just url | not isThereCustomDbUrl -> [(databaseUrlEnvVarName, url)]
      _ -> []
    isThereCustomDbUrl = any ((== databaseUrlEnvVarName) . fst) userEnvVars

dotEnvInServerRootDir :: Path' (Rel ServerRootDir) File'
dotEnvInServerRootDir = [relfile|.env|]

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
      waspProjectDirFromAppComponentDir </> AS.srcTsConfigPath spec

genPackageJson :: AppSpec -> N.NpmDepsFromWasp -> Generator FileDraft
genPackageJson spec waspDependencies = do
  combinedDependencies <- N.genNpmDepsForPackage spec waspDependencies
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|package.json|])
      (C.asServerFile [relfile|package.json|])
      ( Just $
          object
            [ "packageName" .= serverPackageName spec,
              "depsChunk" .= N.getDependenciesPackageJsonEntry combinedDependencies,
              "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry combinedDependencies,
              "nodeVersionRange" .= (">=" <> show NodeVersion.oldestWaspSupportedNodeVersion),
              "startProductionScript"
                .= ( (if hasEntities then "npm run db-migrate-prod && " else "")
                       ++ "NODE_ENV=production npm run start"
                   ),
              "prisma" .= ByteStringLazyUTF8.toString (Aeson.encode $ getPackageJsonPrismaField spec)
            ]
      )
  where
    hasEntities = AS.Util.hasEntities spec

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
              ("express", expressVersionStr),
              ("morgan", "~1.10.0"),
              ("dotenv", "^16.0.2"),
              ("helmet", "^6.0.0"),
              ("superjson", show superjsonVersion)
            ]
            ++ depsRequiredByWebSockets spec,
        N.devDependencies =
          Npm.Dependency.fromList
            [ ("nodemon", "^2.0.19"),
              -- TODO: Allow users to choose whether they want to use TypeScript
              -- in their projects and install these dependencies accordingly.
              ("typescript", show typescriptVersion),
              ("@types/express", show expressTypesVersion),
              ("@types/express-serve-static-core", show expressTypesVersion),
              ("@types/node", "^" <> majorNodeVersionStr <> ".0.0"),
              ("@tsconfig/node" <> majorNodeVersionStr, "latest"),
              ("@types/cors", "^2.8.5"),
              ("rollup", "^4.9.6"),
              ("rollup-plugin-esbuild", "^6.1.1"),
              ("@rollup/plugin-node-resolve", "^16.0.0")
            ],
        peerDependencies = []
      }
  where
    majorNodeVersionStr = show (SV.major $ getLowestNodeVersionUserAllows spec)

genNpmrc :: AppSpec -> Generator [FileDraft]
genNpmrc spec
  -- We only use `.npmrc` to force `npm` to error out if the Node.js version is incompatible.
  --
  -- In dev mode, we already check the Node.js version ourselves before running any `npm` commands,
  -- so we don't need this there.
  --
  -- We do expect users to manually go into the generated directories when bundling the built ouput.
  -- So we do add the `.npmrc` there to help them avoid using an incompatible Node.js version.
  | AS.isBuild spec =
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

genNodemon :: Generator FileDraft
genNodemon =
  return $
    C.mkTmplFdWithData
      [relfile|nodemon.json|]
      (Just $ object ["relativeUserSrcDirPath" .= fromRelDir relativeUserSrcDirPath])
  where
    relativeUserSrcDirPath :: Path' (Rel C.ServerRootDir) (Dir SourceExternalCodeDir) =
      waspProjectDirFromAppComponentDir </> srcDirInWaspProjectDir

genSrcDir :: AppSpec -> Generator [FileDraft]
genSrcDir spec =
  sequence
    [ genFileCopy [relfile|app.js|],
      genServerJs spec
    ]
    <++> genRoutesDir spec
    <++> genOperationsRoutes spec
    <++> genOperations spec
    <++> genAuth spec
    <++> genDbSeed spec
    <++> genMiddleware spec
    <++> genWebSockets spec
  where
    genFileCopy = return . C.mkSrcTmplFd

genServerJs :: AppSpec -> Generator FileDraft
genServerJs spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/server.ts|])
      (C.asServerFile [relfile|src/server.ts|])
      ( Just $
          object
            [ "setupFn" .= extImportToImportJson relPathToServerSrcDir maybeSetupJsFunction,
              "isPgBossJobExecutorUsed" .= isPgBossJobExecutorUsed spec,
              "userWebSocketFn" .= mkWebSocketFnImport maybeWebSocket [reldirP|./|]
            ]
      )
  where
    maybeSetupJsFunction = AS.App.Server.setupFn =<< AS.App.server (snd $ getApp spec)
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec

    relPathToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathToServerSrcDir = [reldirP|./|]

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
          "isAuthEnabled" .= (isAuthEnabled spec :: Bool),
          "areThereAnyCustomApiRoutes" .= (not . null $ AS.getApis spec),
          "areThereAnyCrudRoutes" .= (not . null $ AS.getCruds spec)
        ]

operationsRouteInRootRouter :: String
operationsRouteInRootRouter = "operations"

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

genRollupConfigJs :: AppSpec -> Generator FileDraft
genRollupConfigJs spec =
  return $
    C.mkTmplFdWithData [relfile|rollup.config.js|] (Just tmplData)
  where
    tmplData = object ["areDbSeedsDefined" .= areDbSeedsDefined]

    areDbSeedsDefined = maybe False (not . null) $ getDbSeeds spec
