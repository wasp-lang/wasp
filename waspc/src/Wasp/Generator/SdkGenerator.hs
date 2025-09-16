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
import StrongPath (Abs, Dir, Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import Wasp.AppSpec
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.ExternalFiles as EC
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
import Wasp.Generator.DepVersions (prismaVersion, superjsonVersion)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.FileDraft as FD
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.SdkGenerator.AuthG (genAuth)
import Wasp.Generator.SdkGenerator.Client.AuthG (genNewClientAuth)
import Wasp.Generator.SdkGenerator.Client.CrudG (genNewClientCrudApi)
import qualified Wasp.Generator.SdkGenerator.Client.OperationsGenerator as ClientOpsGen
import Wasp.Generator.SdkGenerator.Client.RouterGenerator (genNewClientRouterApi)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.SdkGenerator.CrudG (genCrud)
import Wasp.Generator.SdkGenerator.DepVersions (tailwindCssVersion)
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
import Wasp.Generator.ServerGenerator.DepVersions
  ( expressTypesVersion,
    expressVersionStr,
  )
import qualified Wasp.Generator.TailwindConfigFile as TCF
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Generator.WebAppGenerator.DepVersions
  ( axiosVersion,
    reactQueryVersion,
    reactRouterVersion,
    reactVersion,
  )
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
      (runNodeCommandAsJob dstDir "npm" ["run", "build"] J.Wasp chan)
  case exitCode of
    ExitSuccess -> return $ Right ()
    ExitFailure code -> return $ Left $ "SDK build failed with exit code: " ++ show code
  where
    dstDir = projectRootDir </> C.sdkRootDirInProjectRootDir

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
      genFileCopy [relfile|client/index.ts|],
      genClientConfigFile,
      genServerConfigFile spec,
      genTsConfigJson,
      genServerUtils spec,
      genPackageJson spec,
      genDbClient spec,
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
    <++> genMiddleware spec
    <++> genExportedTypesDir spec
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
      C.mkTmplFdWithDstAndData
        [relfile|entities/index.ts|]
        [relfile|entities/index.ts|]
        ( Just $
            object
              [ "entities" .= allEntities,
                "isAuthEnabled" .= isJust maybeUserEntityName,
                "authEntityName" .= DbAuth.authEntityName,
                "authIdentityEntityName" .= DbAuth.authIdentityEntityName
              ]
        )
    taggedEntitiesFileDraft =
      C.mkTmplFdWithDstAndData
        [relfile|server/_types/taggedEntities.ts|]
        [relfile|server/_types/taggedEntities.ts|]
        (Just $ object ["entities" .= allEntities])
    typesIndexFileDraft =
      C.mkTmplFdWithDstAndData
        [relfile|server/_types/index.ts|]
        [relfile|server/_types/index.ts|]
        ( Just $
            object
              [ "entities" .= allEntities,
                "isAuthEnabled" .= isJust maybeUserEntityName
              ]
        )
    allEntities = map (makeJsonWithEntityData . fst) $ AS.getEntities spec
    maybeUserEntityName = AS.refName . AS.App.Auth.userEntity <$> AS.App.auth (snd $ AS.Valid.getApp spec)

genPackageJson :: AppSpec -> Generator FileDraft
genPackageJson spec =
  return $
    C.mkTmplFdWithDstAndData
      [relfile|package.json|]
      [relfile|package.json|]
      ( Just $
          object
            [ "depsChunk" .= N.getDependenciesPackageJsonEntry (npmDepsForSdk spec),
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
          [ ("@tanstack/react-query", show reactQueryVersion)
          ]
    }

depsRequiredForTesting :: [Npm.Dependency.Dependency]
depsRequiredForTesting =
  Npm.Dependency.fromList
    [ ("vitest", "^1.2.1"),
      ("@vitest/ui", "^1.2.1"),
      ("jsdom", "^21.1.1"),
      ("@testing-library/react", "^14.1.2"),
      ("@testing-library/jest-dom", "^6.3.0"),
      ("msw", "^1.1.0")
    ]

genClientConfigFile :: Generator FileDraft
genClientConfigFile = return $ C.mkTmplFdWithData relConfigFilePath tmplData
  where
    relConfigFilePath = [relfile|client/config.ts|]
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
genServerConfigFile spec = return $ C.mkTmplFdWithData relConfigFilePath tmplData
  where
    relConfigFilePath = [relfile|server/config.ts|]
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
    C.mkTmplFdWithDstAndData
      [relfile|tsconfig.json|]
      [relfile|tsconfig.json|]
      ( Just $
          object
            [ "majorNodeVersion" .= show (SV.major NodeVersion.oldestWaspSupportedNodeVersion)
            ]
      )

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
genExternalCodeDir :: [EC.CodeFile] -> Generator [FileDraft]
genExternalCodeDir = sequence . mapMaybe genFile

genFile :: EC.CodeFile -> Maybe (Generator FileDraft)
genFile file
  | fileName == "tsconfig.json" = Nothing
  | extension `elem` [".js", ".jsx", ".ts", ".tsx"] = Just $ genSourceFile file
  | otherwise = Just $ genResourceFile file
  where
    extension = FP.takeExtension filePath
    fileName = FP.takeFileName filePath
    filePath = SP.toFilePath $ EC.filePathInExtCodeDir file

genResourceFile :: EC.CodeFile -> Generator FileDraft
genResourceFile file = return $ FD.createCopyFileDraft relDstPath absSrcPath
  where
    relDstPath = C.sdkRootDirInProjectRootDir </> C.extSrcDirInSdkRootDir </> SP.castRel (EC._pathInExtCodeDir file)
    absSrcPath = EC.fileAbsPath file

genSourceFile :: EC.CodeFile -> Generator FD.FileDraft
genSourceFile file = return $ FD.createTextFileDraft relDstPath text
  where
    filePathInSrcExtCodeDir = EC.filePathInExtCodeDir file
    text = EC.fileText file
    relDstPath = C.sdkRootDirInProjectRootDir </> C.extSrcDirInSdkRootDir </> SP.castRel filePathInSrcExtCodeDir

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
genServerUtils spec = return $ C.mkTmplFdWithData [relfile|server/utils.ts|] tmplData
  where
    tmplData = object ["isAuthEnabled" .= (isAuthEnabled spec :: Bool)]

genExportedTypesDir :: AppSpec -> Generator [FileDraft]
genExportedTypesDir _spec =
  return [C.mkTmplFd [relfile|server/types/index.ts|]]

genMiddleware :: AppSpec -> Generator [FileDraft]
genMiddleware _spec =
  sequence
    [ return $ C.mkTmplFd [relfile|server/middleware/index.ts|],
      return $ C.mkTmplFd [relfile|server/middleware/globalMiddleware.ts|]
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
    C.mkTmplFdWithData
      [relfile|server/dbClient.ts|]
      tmplData
  where
    maybePrismaSetupFn = AS.App.db (snd $ AS.Valid.getApp spec) >>= AS.Db.prismaSetupFn

genDevIndex :: Generator FileDraft
genDevIndex =
  return $
    C.mkTmplFdWithData
      [relfile|dev/index.ts|]
      (object ["waspProjectDirFromWebAppDir" .= SP.fromRelDir waspProjectDirFromWebAppDir])
  where
    waspProjectDirFromWebAppDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir) =
      waspProjectDirFromAppComponentDir
