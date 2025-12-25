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
import StrongPath (Abs, Dir, Path', Rel, castRel, fromRelDir, relfile, toFilePath, (</>))
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import Wasp.AppSpec
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
    tailwindCssVersion,
  )
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft, createTextFileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.SdkGenerator.AuthG (genAuth)
import Wasp.Generator.SdkGenerator.Client.AuthG (genNewClientAuth)
import Wasp.Generator.SdkGenerator.Client.CrudG (genNewClientCrudApi)
import qualified Wasp.Generator.SdkGenerator.Client.OperationsGenerator as ClientOpsGen
import Wasp.Generator.SdkGenerator.Client.RouterGenerator (genNewClientRouterApi)
import Wasp.Generator.SdkGenerator.Common
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
import qualified Wasp.Generator.TailwindConfigFile as TCF
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common (WaspProjectDir, waspProjectDirFromAppComponentDir)
import qualified Wasp.Project.Db as Db
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
    sdkRootDir = projectRootDir </> sdkRootDirInProjectRootDir

genSdk :: AppSpec -> Generator [FileDraft]
genSdk spec =
  sequence
    [ return $ makeSdkProjectTmplFd SdkCoreProject [relfile|tsconfig.json|],
      return $ makeSdkProjectTmplFd SdkCoreProject [relfile|server/HttpError.ts|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|tsconfig.json|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|vite-env.d.ts|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|prisma-runtime-library.d.ts|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|api/index.ts|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|api/events.ts|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|core/storage.ts|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|server/index.ts|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|client/test/vitest/helpers.tsx|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|client/test/index.ts|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|client/hooks.ts|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|client/index.ts|],
      genClientConfigFile,
      genServerConfigFile spec,
      genServerUtils spec,
      genDbClient spec,
      genDevIndex
    ]
    <++> genRootSdkFiles spec
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
    <++> genMiddleware
    <++> genExportedTypesDir
    -- New API
    <++> genNewClientAuth spec
    <++> genNewServerApi spec
    <++> genNewServerCrudApi spec
    <++> genNewClientCrudApi spec
    <++> genNewEmailSenderApi spec
    <++> genNewJobsApi spec
    <++> genNewClientRouterApi spec
    <++> genEnvValidation spec

genRootSdkFiles :: AppSpec -> Generator [FileDraft]
genRootSdkFiles spec =
  sequence
    [ return $ makeSdkRootTmplFile [relfile|tsconfig.json|],
      return $ makeSdkRootTmplFile [relfile|tsconfig.sdk.json|],
      return $ makeSdkRootTmplFile [relfile|copy-assets.js|],
      genPackageJson spec
    ]

genPackageJson :: AppSpec -> Generator FileDraft
genPackageJson spec =
  return $
    makeSdkRootTmplFileWithData
      [relfile|package.json|]
      ( object
          [ "depsChunk" .= N.getDependenciesPackageJsonEntry (npmDepsForSdk spec),
            "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry (npmDepsForSdk spec),
            "peerDepsChunk" .= N.getPeerDependenciesPackageJsonEntry (npmDepsForSdk spec)
          ]
      )

genEntitiesAndServerTypesDirs :: AppSpec -> Generator [FileDraft]
genEntitiesAndServerTypesDirs spec =
  return
    [ entitiesIndexFileDraft,
      taggedEntitiesFileDraft,
      typesIndexFileDraft
    ]
  where
    entitiesIndexFileDraft =
      makeSdkProjectTmplFdWithData
        SdkUserCoreProject
        [relfile|entities/index.ts|]
        ( object
            [ "entities" .= allEntities,
              "isAuthEnabled" .= isJust maybeUserEntityName,
              "authEntityName" .= DbAuth.authEntityName,
              "authIdentityEntityName" .= DbAuth.authIdentityEntityName
            ]
        )
    taggedEntitiesFileDraft =
      makeSdkProjectTmplFdWithData
        SdkUserCoreProject
        [relfile|server/_types/taggedEntities.ts|]
        (object ["entities" .= allEntities])
    typesIndexFileDraft =
      makeSdkProjectTmplFdWithData
        SdkUserCoreProject
        [relfile|server/_types/index.ts|]
        ( object
            [ "entities" .= allEntities,
              "isAuthEnabled" .= isJust maybeUserEntityName
            ]
        )
    allEntities = map (makeJsonWithEntityData . fst) $ AS.getEntities spec
    maybeUserEntityName = AS.refName . AS.App.Auth.userEntity <$> AS.App.auth (snd $ AS.Valid.getApp spec)

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
            ("react-router-dom", show reactRouterVersion),
            ("react-hook-form", "^7.45.4"),
            ("superjson", show superjsonVersion)
          ]
          ++ depsRequiredForAuth spec
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
          -- These deps need to be installed in the SDK becasue when we run client tests,
          -- we are running them from the project root dir and PostCSS and Tailwind
          -- can't be resolved from WebApp node_modules, so we need to install them in the SDK.
          ++ depsRequiredByTailwind spec
          ++ depsRequiredByEnvValidation,
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

depsRequiredForTesting :: [Npm.Dependency.Dependency]
depsRequiredForTesting =
  Npm.Dependency.fromList
    [ ("vitest", "^1.2.1"),
      ("@vitest/ui", "^1.2.1"),
      ("jsdom", "^21.1.1"),
      ("@testing-library/react", "^16.3.0"),
      ("@testing-library/jest-dom", "^6.3.0"),
      ("msw", "^1.1.0")
    ]

genClientConfigFile :: Generator FileDraft
genClientConfigFile =
  return $
    makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = [relfile|client/config.ts|]
    tmplData =
      object
        [ "serverUrlEnvVarName" .= WebApp.serverUrlEnvVarName
        ]

genCoreSerializationDir :: AppSpec -> Generator [FileDraft]
genCoreSerializationDir spec =
  return $
    [ makeSdkProjectTmplFd SdkUserCoreProject [relfile|core/serialization/custom-register.ts|],
      makeSdkProjectTmplFdWithData SdkUserCoreProject [relfile|core/serialization/index.ts|] tmplData
    ]
      ++ maybeToList prismaSerializationFile
  where
    tmplData =
      object
        [ "entitiesExist" .= entitiesExist
        ]

    prismaSerializationFile
      | entitiesExist = Just $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|core/serialization/prisma.ts|]
      | otherwise = Nothing

    entitiesExist = hasEntities spec

genServerConfigFile :: AppSpec -> Generator FileDraft
genServerConfigFile spec = return $ makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = [relfile|server/config.ts|]
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabled spec,
          "clientUrlEnvVarName" .= Server.clientUrlEnvVarName,
          "serverUrlEnvVarName" .= Server.serverUrlEnvVarName,
          "jwtSecretEnvVarName" .= AuthG.jwtSecretEnvVarName,
          "databaseUrlEnvVarName" .= Db.databaseUrlEnvVarName
        ]

depsRequiredForAuth :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredForAuth spec = maybe [] (const authDeps) maybeAuth
  where
    maybeAuth = AS.App.auth $ snd $ AS.Valid.getApp spec
    authDeps =
      Npm.Dependency.fromList
        [ -- Argon2 is used for hashing passwords.
          ("@node-rs/argon2", "^1.8.3")
        ]

depsRequiredByTailwind :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByTailwind spec =
  if TCF.isTailwindUsed spec
    then
      Npm.Dependency.fromList
        [ ("tailwindcss", show tailwindCssVersion),
          ("postcss", "^8.4.21"),
          ("autoprefixer", "^10.4.13")
        ]
    else []

-- TODO(filip): Figure out where this belongs. Check https://github.com/wasp-lang/wasp/pull/1602#discussion_r1437144166 .
-- Also, fix imports for wasp project.
installNpmDependencies :: Path' Abs (Dir WaspProjectDir) -> J.Job
installNpmDependencies projectDir =
  runNodeCommandAsJob projectDir "npm" ["install"] J.Wasp

-- todo(filip): consider reorganizing/splitting the file.

-- | Takes external code files from Wasp and generates them in new location as part of the generated project.
-- It might not just copy them but also do some changes on them, as needed.
genExternalCodeDir :: [EF.CodeFile] -> Generator [FileDraft]
genExternalCodeDir = sequence . mapMaybe genExternalFile

genExternalFile :: EF.CodeFile -> Maybe (Generator FileDraft)
genExternalFile file
  | fileName == "tsconfig.json" = Nothing
  | extension `elem` [".js", ".jsx", ".ts", ".tsx"] = Just $ genExternalSourceFile file
  | otherwise = Just $ genExternalResourceFile file
  where
    extension = FP.takeExtension filePath
    fileName = FP.takeFileName filePath
    filePath = toFilePath $ EF.filePathInExtCodeDir file

genExternalResourceFile :: EF.CodeFile -> Generator FileDraft
genExternalResourceFile file = return $ createCopyFileDraft destFile srcFile
  where
    destFile =
      sdkRootDirInProjectRootDir
        </> extSrcDirInSdkRootDir
        </> castRel (EF.filePathInExtCodeDir file)
    srcFile = EF.fileAbsPath file

genExternalSourceFile :: EF.CodeFile -> Generator FileDraft
genExternalSourceFile file = return $ createTextFileDraft destFile srcFile
  where
    destFile =
      sdkRootDirInProjectRootDir
        </> extSrcDirInSdkRootDir
        </> castRel (EF.filePathInExtCodeDir file)
    srcFile = EF.fileText file

genUniversalDir :: Generator [FileDraft]
genUniversalDir =
  return
    [ makeSdkProjectTmplFd SdkUserCoreProject [relfile|universal/url.ts|],
      makeSdkProjectTmplFd SdkUserCoreProject [relfile|universal/types.ts|],
      makeSdkProjectTmplFd SdkUserCoreProject [relfile|universal/validators.ts|],
      makeSdkProjectTmplFd SdkUserCoreProject [relfile|universal/predicates.ts|],
      makeSdkProjectTmplFd SdkUserCoreProject [relfile|universal/ansiColors.ts|]
    ]

genServerUtils :: AppSpec -> Generator FileDraft
genServerUtils spec = return $ makeSdkProjectTmplFdWithData SdkUserCoreProject [relfile|server/utils.ts|] tmplData
  where
    tmplData = object ["isAuthEnabled" .= (isAuthEnabled spec :: Bool)]

genExportedTypesDir :: Generator [FileDraft]
genExportedTypesDir =
  return [makeSdkProjectTmplFd SdkUserCoreProject [relfile|server/types/index.ts|]]

genMiddleware :: Generator [FileDraft]
genMiddleware =
  sequence
    [ return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|server/middleware/index.ts|],
      return $ makeSdkProjectTmplFd SdkUserCoreProject [relfile|server/middleware/globalMiddleware.ts|]
    ]

genDbClient :: AppSpec -> Generator FileDraft
genDbClient spec = do
  areThereAnyEntitiesDefined <- not . null <$> getEntitiesForPrismaSchema spec
  let tmplData =
        object
          [ "areThereAnyEntitiesDefined" .= areThereAnyEntitiesDefined,
            "prismaSetupFn" .= extImportToImportJson maybePrismaSetupFn
          ]

  return $
    makeSdkProjectTmplFdWithData
      SdkUserCoreProject
      [relfile|server/dbClient.ts|]
      tmplData
  where
    maybePrismaSetupFn = AS.App.db (snd $ AS.Valid.getApp spec) >>= AS.Db.prismaSetupFn

genDevIndex :: Generator FileDraft
genDevIndex =
  return $
    makeSdkProjectTmplFdWithData
      SdkUserCoreProject
      [relfile|dev/index.ts|]
      (object ["waspProjectDirFromWebAppDir" .= fromRelDir waspProjectDirFromWebAppDir])
  where
    waspProjectDirFromWebAppDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir) =
      waspProjectDirFromAppComponentDir
