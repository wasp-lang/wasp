{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator
  ( genSdk,
    installNpmDependencies,
    genExternalCodeDir,
    buildSdk,
    npmDepsForSdk,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Data.Aeson (object)
import Data.Aeson.Types ((.=))
import Data.Maybe (isJust, mapMaybe, maybeToList)
import StrongPath (Abs, Dir, Path', Rel, castRel, fromRelDir, fromRelFile, relfile, (</>))
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.ExternalFiles as EF
import Wasp.AppSpec.Util (hasEntities)
import Wasp.AppSpec.Valid (isAuthEnabled)
import qualified Wasp.AppSpec.Valid as AS.Valid
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Common
  ( ProjectRootDir,
    WebAppRootDir,
    makeJsonWithEntityData,
  )
import Wasp.Generator.DbGenerator (getEntitiesForPrismaSchema)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.DepVersions
  ( axiosVersion,
    expressTypesVersion,
    expressVersionStr,
    prismaVersion,
    reactQueryVersion,
    reactRouterVersion,
    reactVersion,
    superjsonVersion,
  )
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.SdkGenerator.AuthG (genAuth)
import Wasp.Generator.SdkGenerator.Client.AuthG (genNewClientAuth)
import Wasp.Generator.SdkGenerator.Client.CrudG (genNewClientCrudApi)
import qualified Wasp.Generator.SdkGenerator.Client.OperationsGenerator as ClientOpsGen
import Wasp.Generator.SdkGenerator.Client.RouterGenerator (genNewClientRouterApi)
import Wasp.Generator.SdkGenerator.Common (sdkPackageName)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.SdkGenerator.CrudG (genCrud)
import Wasp.Generator.SdkGenerator.EnvValidation (depsRequiredByEnvValidation, genEnvValidation)
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.Server.AuthG (genNewServerApi)
import Wasp.Generator.SdkGenerator.Server.CrudG (genNewServerCrudApi)
import Wasp.Generator.SdkGenerator.Server.EmailSenderG (depsRequiredByEmail, genNewEmailSenderApi)
import Wasp.Generator.SdkGenerator.Server.JobGenerator (depsRequiredByJobs, genNewJobsApi)
import Wasp.Generator.SdkGenerator.Server.OAuthG (depsRequiredByOAuth)
import qualified Wasp.Generator.SdkGenerator.Server.OperationsGenerator as ServerOpsGen
import Wasp.Generator.SdkGenerator.ServerApiG (genServerApi)
import Wasp.Generator.SdkGenerator.WebSocketGenerator (depsRequiredByWebSockets, genWebSockets)
import qualified Wasp.Generator.ServerGenerator.AuthG as AuthG
import qualified Wasp.Generator.ServerGenerator.AuthG as ServerAuthG
import qualified Wasp.Generator.ServerGenerator.Common as Server
import Wasp.Generator.WaspLibs.AvailableLibs (waspLibs)
import Wasp.Generator.WaspLibs.Common (libsRootDirFromSdkDir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (WaspProjectDir, waspProjectDirFromAppComponentDir)
import qualified Wasp.Project.Db as Db
import qualified Wasp.SemanticVersion.Version as SV
  ( Version (major),
  )
import Wasp.Util ((<++>))

buildSdk :: Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
buildSdk projectRootDir = do
  chan <- newChan
  (_, exitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (runNodeCommandAsJob sdkRootDir "npm" ["run", "build"] J.Wasp chan)
  return $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure code -> Left $ "SDK build failed with exit code: " ++ show code
  where
    sdkRootDir = projectRootDir </> C.sdkRootDirInGeneratedCodeDir

genSdk :: AppSpec -> Generator [FileDraft]
genSdk spec =
  sequence
    [ genFileCopy [relfile|vite-env.d.ts|],
      genFileCopy [relfile|prisma-runtime-library.d.ts|],
      genFileCopy [relfile|scripts/copy-assets.js|],
      genFileCopy [relfile|api/index.ts|],
      genFileCopy [relfile|api/events.ts|],
      genFileCopy [relfile|core/storage.ts|],
      genFileCopy [relfile|server/index.ts|],
      genFileCopy [relfile|server/HttpError.ts|],
      genFileCopy [relfile|client/test/vitest/helpers.tsx|],
      genFileCopy [relfile|client/test/index.ts|],
      genFileCopy [relfile|client/hooks.ts|],
      genFileCopy [relfile|client/index.ts|],
      genClientConfigFile,
      genServerConfigFile spec,
      genTsConfigJson,
      genServerUtils spec,
      genPackageJson spec,
      genServerDbClient spec,
      genDevIndex
    ]
    <++> ServerOpsGen.genOperations spec
    <++> ClientOpsGen.genOperations spec
    <++> genAuth spec
    <++> genUniversalDir
    <++> genExternalCodeDir (AS.externalCodeFiles spec)
    <++> genEntitiesAndServerTypesDirs spec
    <++> genCoreSerializationDir spec
    <++> genCrud spec
    <++> genServerApi spec
    <++> genWebSockets spec
    <++> genServerMiddleware
    <++> genServerExportedTypesDir
    -- New API
    <++> genNewClientAuth spec
    <++> genNewServerApi spec
    <++> genNewServerCrudApi spec
    <++> genNewClientCrudApi spec
    <++> genNewEmailSenderApi spec
    <++> genNewJobsApi spec
    <++> genNewClientRouterApi spec
    <++> genEnvValidation spec
  where
    genFileCopy = return . C.mkTmplFd

genEntitiesAndServerTypesDirs :: AppSpec -> Generator [FileDraft]
genEntitiesAndServerTypesDirs spec =
  return
    [ entitiesIndexFileDraft,
      taggedEntitiesFileDraft,
      typesIndexFileDraft
    ]
  where
    entitiesIndexFileDraft =
      C.mkTmplFdWithData
        [relfile|entities/index.ts|]
        ( object
            [ "entities" .= allEntities,
              "isAuthEnabled" .= isJust maybeUserEntityName,
              "authEntityName" .= DbAuth.authEntityName,
              "authIdentityEntityName" .= DbAuth.authIdentityEntityName
            ]
        )
    taggedEntitiesFileDraft =
      C.mkTmplFdWithData
        [relfile|server/_types/taggedEntities.ts|]
        (object ["entities" .= allEntities])
    typesIndexFileDraft =
      C.mkTmplFdWithData
        [relfile|server/_types/index.ts|]
        ( object
            [ "entities" .= allEntities,
              "isAuthEnabled" .= isJust maybeUserEntityName
            ]
        )
    allEntities = map (makeJsonWithEntityData . fst) $ AS.getEntities spec
    maybeUserEntityName = AS.refName . AS.App.Auth.userEntity <$> AS.App.auth (snd $ AS.Valid.getApp spec)

genPackageJson :: AppSpec -> Generator FileDraft
genPackageJson spec = do
  return $
    C.mkTmplFdWithData
      [relfile|package.json|]
      ( object
          [ "sdkPackageName" .= sdkPackageName,
            "depsChunk" .= N.getDependenciesPackageJsonEntry (npmDepsForSdk spec),
            "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry (npmDepsForSdk spec),
            "peerDepsChunk" .= N.getPeerDependenciesPackageJsonEntry (npmDepsForSdk spec)
          ]
      )

npmDepsForSdk :: AppSpec -> N.NpmDepsForPackage
npmDepsForSdk spec =
  N.NpmDepsForPackage
    { N.dependencies =
        Npm.Dependency.fromList
          [ ("@prisma/client", show prismaVersion),
            ("prisma", show prismaVersion),
            ("axios", show axiosVersion),
            ("express", expressVersionStr),
            ("mitt", "3.0.0"),
            ("react", show reactVersion),
            ("react-router", show reactRouterVersion),
            ("react-hook-form", "^7.45.4"),
            ("superjson", show superjsonVersion)
          ]
          ++ depsRequiredByOAuth spec
          -- Server auth deps must be installed in the SDK because "@lucia-auth/adapter-prisma"
          -- lists prisma/client as a dependency.
          -- Installing it inside .wasp/out/server/node_modules would also
          -- install prisma/client in the same folder, which would cause our
          -- runtime to load the wrong (uninitialized prisma/client).
          -- TODO(filip): Find a better way to handle duplicate
          -- dependencies: https://github.com/wasp-lang/wasp/issues/1640
          ++ ServerAuthG.depsRequiredByAuth spec
          ++ depsRequiredByEmail spec
          ++ depsRequiredByWebSockets spec
          ++ depsRequiredForTesting
          ++ depsRequiredByJobs spec
          ++ depsRequiredByEnvValidation
          ++ waspLibsNpmDeps,
      N.devDependencies =
        Npm.Dependency.fromList
          [ -- Should @types/* go into their package.json?
            ("@types/express", show expressTypesVersion),
            ("@types/express-serve-static-core", show expressTypesVersion)
          ],
      N.peerDependencies =
        Npm.Dependency.fromList
          [ ("@tanstack/react-query", reactQueryVersion)
          ]
    }
  where
    waspLibsNpmDeps = map (WaspLib.makeLocalNpmDepFromWaspLib libsRootDirFromSdkDir) waspLibs

depsRequiredForTesting :: [Npm.Dependency.Dependency]
depsRequiredForTesting =
  Npm.Dependency.fromList
    [ ("vitest", "^4.0.16"),
      ("@vitest/ui", "^4.0.16"),
      ("jsdom", "^27.4.0"),
      ("@testing-library/react", "^16.3.1"),
      ("@testing-library/jest-dom", "^6.9.1"),
      ("msw", "^2.12.7")
    ]

genClientConfigFile :: Generator FileDraft
genClientConfigFile =
  return $ C.mkTmplFdWithData [relfile|client/config.ts|] tmplData
  where
    tmplData =
      object
        [ "serverUrlEnvVarName" .= WebApp.serverUrlEnvVarName
        ]

genCoreSerializationDir :: AppSpec -> Generator [FileDraft]
genCoreSerializationDir spec =
  return $
    [ C.mkTmplFd [relfile|core/serialization/custom-register.ts|],
      C.mkTmplFdWithData [relfile|core/serialization/index.ts|] tmplData
    ]
      ++ maybeToList prismaSerializationFile
  where
    tmplData =
      object
        [ "entitiesExist" .= entitiesExist
        ]

    prismaSerializationFile
      | entitiesExist = Just $ C.mkTmplFd [relfile|core/serialization/prisma.ts|]
      | otherwise = Nothing

    entitiesExist = hasEntities spec

genServerConfigFile :: AppSpec -> Generator FileDraft
genServerConfigFile spec = return $ C.mkTmplFdWithData [relfile|server/config.ts|] tmplData
  where
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabled spec,
          "clientUrlEnvVarName" .= Server.clientUrlEnvVarName,
          "serverUrlEnvVarName" .= Server.serverUrlEnvVarName,
          "jwtSecretEnvVarName" .= AuthG.jwtSecretEnvVarName,
          "databaseUrlEnvVarName" .= Db.databaseUrlEnvVarName
        ]

-- todo(filip): remove this duplication, we have almost the same thing in the
-- ServerGenerator.
genTsConfigJson :: Generator FileDraft
genTsConfigJson = do
  return $
    C.mkTmplFdWithData
      [relfile|tsconfig.json|]
      ( object
          [ "majorNodeVersion" .= show (SV.major NodeVersion.oldestWaspSupportedNodeVersion)
          ]
      )

-- TODO(filip): Figure out where this belongs.
-- Check https://github.com/wasp-lang/wasp/pull/1602#discussion_r1437144166 .
-- Also, fix imports for wasp project.
installNpmDependencies :: Path' Abs (Dir WaspProjectDir) -> J.Job
installNpmDependencies projectDir =
  runNodeCommandAsJob projectDir "npm" ["install"] J.Wasp

-- todo(filip): consider reorganizing/splitting the file.

-- | Takes external code files from Wasp,
-- and generates them in a new location as part of the generated project.
-- It might not just copy them but also do some changes on them, as needed.
genExternalCodeDir :: [EF.CodeFile] -> Generator [FileDraft]
genExternalCodeDir = sequence . mapMaybe genExternalFile

genExternalFile :: EF.CodeFile -> Maybe (Generator FileDraft)
genExternalFile file
  | fileName == "tsconfig.json" = Nothing
  | otherwise = Just . return . createCopyFileDraft destFile . EF.fileAbsPath $ file
  where
    fileName = FP.takeFileName . fromRelFile $ externalFilePath
    destFile =
      C.sdkRootDirInGeneratedCodeDir
        </> C.extSrcDirInSdkRootDir
        </> castRel externalFilePath

    externalFilePath = EF.filePathInExtCodeDir file

genUniversalDir :: Generator [FileDraft]
genUniversalDir =
  return
    [ C.mkTmplFd [relfile|universal/url.ts|],
      C.mkTmplFd [relfile|universal/types.ts|],
      C.mkTmplFd [relfile|universal/validators.ts|],
      C.mkTmplFd [relfile|universal/predicates.ts|],
      C.mkTmplFd [relfile|universal/ansiColors.ts|]
    ]

genServerUtils :: AppSpec -> Generator FileDraft
genServerUtils spec =
  return $ C.mkTmplFdWithData [relfile|server/utils.ts|] tmplData
  where
    tmplData = object ["isAuthEnabled" .= (isAuthEnabled spec :: Bool)]

genServerExportedTypesDir :: Generator [FileDraft]
genServerExportedTypesDir =
  return [C.mkTmplFd [relfile|server/types/index.ts|]]

genServerMiddleware :: Generator [FileDraft]
genServerMiddleware =
  sequence
    [ return $ C.mkTmplFd [relfile|server/middleware/index.ts|],
      return $ C.mkTmplFd [relfile|server/middleware/globalMiddleware.ts|]
    ]

genServerDbClient :: AppSpec -> Generator FileDraft
genServerDbClient spec = do
  areThereAnyEntitiesDefined <- not . null <$> getEntitiesForPrismaSchema spec
  let tmplData =
        object
          [ "areThereAnyEntitiesDefined" .= areThereAnyEntitiesDefined,
            "prismaSetupFn" .= extImportToImportJson maybePrismaSetupFn
          ]

  return $
    C.mkTmplFdWithData
      [relfile|server/dbClient.ts|]
      tmplData
  where
    maybePrismaSetupFn = AS.App.db (snd $ AS.Valid.getApp spec) >>= AS.Db.prismaSetupFn

genDevIndex :: Generator FileDraft
genDevIndex =
  return $ C.mkTmplFdWithData [relfile|dev/index.ts|] tmplData
  where
    tmplData = object ["waspProjectDirFromWebAppDir" .= fromRelDir waspProjectDirFromWebAppDir]

    waspProjectDirFromWebAppDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir)
    waspProjectDirFromWebAppDir = waspProjectDirFromAppComponentDir
