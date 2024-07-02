{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Wasp.Generator.ServerGenerator
  ( genServer,
    operationsRouteInRootRouter,
    npmDepsForWasp,
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
    File',
    Path,
    Path',
    Posix,
    Rel,
    reldirP,
    relfile,
    (</>),
  )
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import Wasp.AppSpec.Valid (getApp, getLowestNodeVersionUserAllows, isAuthEnabled)
import Wasp.Env (envVarsToDotEnvContent)
import Wasp.Generator.Common
  ( ServerRootDir,
  )
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.ServerGenerator.ApiRoutesG (genApis)
import Wasp.Generator.ServerGenerator.Auth.OAuthAuthG (depsRequiredByOAuth)
import Wasp.Generator.ServerGenerator.AuthG (genAuth)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.CrudG (genCrud)
import Wasp.Generator.ServerGenerator.Db.Seed (genDbSeed, getDbSeeds, getPackageJsonPrismaSeedField)
import Wasp.Generator.ServerGenerator.JobGenerator (genJobs)
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson, getAliasedJsImportStmtAndIdentifier)
import Wasp.Generator.ServerGenerator.OperationsG (genOperations)
import Wasp.Generator.ServerGenerator.OperationsRoutesG (genOperationsRoutes)
import Wasp.Generator.ServerGenerator.WebSocketG (depsRequiredByWebSockets, genWebSockets, mkWebSocketFnImport)
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Db (databaseUrlEnvVarName)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util ((<++>))

genServer :: AppSpec -> Generator [FileDraft]
genServer spec =
  sequence
    [ genFileCopy [relfile|README.md|],
      genFileCopy [relfile|nodemon.json|],
      genRollupConfigJs spec,
      genTsConfigJson,
      genPackageJson spec (npmDepsForWasp spec),
      genNpmrc,
      genGitignore
    ]
    <++> genSrcDir spec
    <++> genDotEnv spec
    <++> genJobs spec
    <++> genEnvValidationScript
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

genTsConfigJson :: Generator FileDraft
genTsConfigJson = do
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|tsconfig.json|])
      (C.asServerFile [relfile|tsconfig.json|])
      ( Just $
          object
            [ "majorNodeVersion" .= show (SV.major NodeVersion.oldestWaspSupportedNodeVersion)
            ]
      )

genPackageJson :: AppSpec -> N.NpmDepsForWasp -> Generator FileDraft
genPackageJson spec waspDependencies = do
  combinedDependencies <- N.genNpmDepsForPackage spec waspDependencies
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|package.json|])
      (C.asServerFile [relfile|package.json|])
      ( Just $
          object
            [ "depsChunk" .= N.getDependenciesPackageJsonEntry combinedDependencies,
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
    hasEntities = not . null $ AS.getEntities spec

getPackageJsonPrismaField :: AppSpec -> Aeson.Value
getPackageJsonPrismaField spec = object $ [] <> seedEntry
  where
    seedEntry = maybeToList $ Just . ("seed" .=) =<< getPackageJsonPrismaSeedField spec

npmDepsForWasp :: AppSpec -> N.NpmDepsForWasp
npmDepsForWasp spec =
  N.NpmDepsForWasp
    { N.waspDependencies =
        AS.Dependency.fromList
          [ ("cookie-parser", "~1.4.6"),
            ("cors", "^2.8.5"),
            ("express", "~4.18.1"),
            ("morgan", "~1.10.0"),
            ("dotenv", "16.0.2"),
            ("helmet", "^6.0.0"),
            ("rate-limiter-flexible", "^2.4.1"),
            ("superjson", "^1.12.2")
          ]
          ++ depsRequiredByOAuth spec
          ++ depsRequiredByWebSockets spec,
      N.waspDevDependencies =
        AS.Dependency.fromList
          [ ("nodemon", "^2.0.19"),
            ("standard", "^17.0.0"),
            -- TODO: Allow users to choose whether they want to use TypeScript
            -- in their projects and install these dependencies accordingly.
            ("typescript", "^5.1.0"),
            ("@types/express", "^4.17.13"),
            ("@types/express-serve-static-core", "^4.17.13"),
            ("@types/node", "^" <> majorNodeVersionStr <> ".0.0"),
            ("@tsconfig/node" <> majorNodeVersionStr, "latest"),
            ("@types/cors", "^2.8.5"),
            ("rollup", "^4.9.6"),
            ("rollup-plugin-esbuild", "^6.1.1")
          ]
    }
  where
    majorNodeVersionStr = show (SV.major $ getLowestNodeVersionUserAllows spec)

genNpmrc :: Generator FileDraft
genNpmrc =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|npmrc|])
      (C.asServerFile [relfile|.npmrc|])
      Nothing

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
    [ genFileCopy [relfile|app.js|],
      genServerJs spec,
      genFileCopy [relfile|polyfill.ts|]
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
          "isAuthEnabled" .= (isAuthEnabled spec :: Bool),
          "areThereAnyCustomApiRoutes" .= (not . null $ AS.getApis spec),
          "areThereAnyCrudRoutes" .= (not . null $ AS.getCruds spec)
        ]

operationsRouteInRootRouter :: String
operationsRouteInRootRouter = "operations"

genEnvValidationScript :: Generator [FileDraft]
genEnvValidationScript =
  return
    [ C.mkTmplFd [relfile|scripts/validate-env.mjs|]
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

genRollupConfigJs :: AppSpec -> Generator FileDraft
genRollupConfigJs spec =
  return $
    C.mkTmplFdWithData [relfile|rollup.config.js|] (Just tmplData)
  where
    tmplData = object ["areDbSeedsDefined" .= areDbSeedsDefined]

    areDbSeedsDefined = maybe False (not . null) $ getDbSeeds spec
