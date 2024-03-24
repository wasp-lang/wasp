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
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import StrongPath
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import Wasp.AppSpec
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.Entity as AS.Entity
import qualified Wasp.AppSpec.ExternalFiles as EC
import Wasp.AppSpec.Valid (getLowestNodeVersionUserAllows, isAuthEnabled)
import qualified Wasp.AppSpec.Valid as AS.Valid
import Wasp.Generator.Common (ProjectRootDir, makeJsonWithEntityData, prismaVersion)
import qualified Wasp.Generator.ConfigFile as G.CF
import Wasp.Generator.DbGenerator (getEntitiesForPrismaSchema)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.FileDraft as FD
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.SdkGenerator.AuthG (genAuth)
import Wasp.Generator.SdkGenerator.Client.AuthG (genNewClientAuth)
import Wasp.Generator.SdkGenerator.Client.CrudG (genNewClientCrudApi)
import qualified Wasp.Generator.SdkGenerator.Client.OperationsGenerator as ClientOpsGen
import Wasp.Generator.SdkGenerator.Client.RouterGenerator (genNewClientRouterApi)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.SdkGenerator.CrudG (genCrud)
import Wasp.Generator.SdkGenerator.Server.AuthG (genNewServerApi)
import Wasp.Generator.SdkGenerator.Server.CrudG (genNewServerCrudApi)
import Wasp.Generator.SdkGenerator.Server.EmailSenderG (depsRequiredByEmail, genNewEmailSenderApi)
import Wasp.Generator.SdkGenerator.Server.JobGenerator (depsRequiredByJobs, genNewJobsApi)
import qualified Wasp.Generator.SdkGenerator.Server.OperationsGenerator as ServerOpsGen
import Wasp.Generator.SdkGenerator.ServerApiG (genServerApi)
import Wasp.Generator.SdkGenerator.WebSocketGenerator (depsRequiredByWebSockets, genWebSockets)
import qualified Wasp.Generator.ServerGenerator.AuthG as ServerAuthG
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Project.Db as Db
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (toLowerFirst, (<++>))

genSdk :: AppSpec -> Generator [FileDraft]
genSdk spec = genSdkReal spec

buildSdk :: Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
buildSdk projectRootDir = do
  chan <- newChan
  (_, exitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (runNodeCommandAsJob dstDir "npx" ["tsc"] J.Wasp chan)
  case exitCode of
    ExitSuccess -> return $ Right ()
    ExitFailure code -> return $ Left $ "SDK build failed with exit code: " ++ show code
  where
    dstDir = projectRootDir </> C.sdkRootDirInProjectRootDir

genSdkReal :: AppSpec -> Generator [FileDraft]
genSdkReal spec =
  sequence
    [ genFileCopy [relfile|vite-env.d.ts|],
      genFileCopy [relfile|api/index.ts|],
      genFileCopy [relfile|api/events.ts|],
      genFileCopy [relfile|core/storage.ts|],
      genFileCopy [relfile|server/index.ts|],
      genFileCopy [relfile|server/HttpError.ts|],
      genFileCopy [relfile|client/test/vitest/helpers.tsx|],
      genFileCopy [relfile|client/test/index.ts|],
      genFileCopy [relfile|client/index.ts|],
      genFileCopy [relfile|dev/index.ts|],
      genClientConfigFile,
      genServerConfigFile spec,
      genTsConfigJson,
      genServerUtils spec,
      genPackageJson spec,
      genDbClient spec
    ]
    <++> ServerOpsGen.genOperations spec
    <++> ClientOpsGen.genOperations spec
    <++> genAuth spec
    <++> genUniversalDir
    <++> genExternalCodeDir (AS.externalCodeFiles spec)
    <++> genEntitiesAndServerTypesDirs spec
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
  where
    genFileCopy = return . C.mkTmplFd

-- genSdkHardcoded :: Generator [FileDraft]
-- genSdkHardcoded =
--   return []
--   where
--     copyFile = C.mkTmplFd
--     copyFolder :: Path' (Rel SdkTemplatesDir) (Dir d) -> FileDraft
--     copyFolder modul =
--       createCopyDirFileDraft
--         RemoveExistingDstDir
--         (dstFolder </> castRel modul)
--         (srcFolder </> modul)
--     dstFolder = C.sdkRootDirInProjectRootDir
--     srcFolder = absSdkTemplatesDir
--     absSdkTemplatesDir = unsafePerformIO getTemplatesDirAbsPath </> C.sdkTemplatesDirInTemplatesDir

genEntitiesAndServerTypesDirs :: AppSpec -> Generator [FileDraft]
genEntitiesAndServerTypesDirs spec =
  return
    [ entitiesIndexFileDraft,
      taggedEntitiesFileDraft,
      serializationFileDraft,
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
    serializationFileDraft =
      C.mkTmplFd
        [relfile|server/_types/serialization.ts|]
    typesIndexFileDraft =
      C.mkTmplFdWithDstAndData
        [relfile|server/_types/index.ts|]
        [relfile|server/_types/index.ts|]
        ( Just $
            object
              [ "entities" .= allEntities,
                "isAuthEnabled" .= isJust maybeUserEntityName,
                "authIdentityEntityName" .= DbAuth.authIdentityEntityName,
                "userFieldName" .= toLowerFirst userEntityName
              ]
        )
    userEntityName = fromMaybe "" maybeUserEntityName
    allEntities = map (makeJsonWithEntityData . fst) $ AS.getDecls @AS.Entity.Entity spec
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
              "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry (npmDepsForSdk spec)
            ]
      )

npmDepsForSdk :: AppSpec -> N.NpmDepsForPackage
npmDepsForSdk spec =
  N.NpmDepsForPackage
    { N.dependencies =
        AS.Dependency.fromList
          [ ("@prisma/client", show prismaVersion),
            ("prisma", show prismaVersion),
            ("@tanstack/react-query", "^4.29.0"),
            ("axios", "^1.4.0"),
            ("express", "~4.18.1"),
            ("mitt", "3.0.0"),
            ("react", "^18.2.0"),
            ("lodash.merge", "^4.6.2"),
            ("react-router-dom", "^5.3.3"),
            ("react-hook-form", "^7.45.4"),
            ("superjson", "^1.12.2"),
            -- Todo: why is this in dependencies, should it be in dev dependencies?
            -- Should it go into their package.json
            ("@types/express-serve-static-core", "^4.17.13"),
            ("@types/react-router-dom", "^5.3.3")
          ]
          ++ depsRequiredForAuth spec
          -- This must be installed in the SDK because it lists prisma/client as a dependency.
          -- Installing it inside .wasp/out/server/node_modules would also
          -- install prisma/client in the same folder, which would cause our
          -- runtime to load the wrong (uninitialized prisma/client)
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
          ++ depsRequiredByTailwind spec,
      N.devDependencies =
        AS.Dependency.fromList
          [ ("@tsconfig/node" <> majorNodeVersionStr, "latest")
          ]
    }
  where
    majorNodeVersionStr = show (SV.major $ getLowestNodeVersionUserAllows spec)

depsRequiredForTesting :: [AS.Dependency.Dependency]
depsRequiredForTesting =
  AS.Dependency.fromList
    [ ("vitest", "^1.2.1"),
      ("@vitest/ui", "^1.2.1"),
      ("jsdom", "^21.1.1"),
      ("@testing-library/react", "^14.1.2"),
      ("@testing-library/jest-dom", "^6.3.0"),
      ("msw", "^1.1.0")
    ]

genServerConfigFile :: AppSpec -> Generator FileDraft
genServerConfigFile spec = return $ C.mkTmplFdWithData relConfigFilePath tmplData
  where
    relConfigFilePath = [relfile|server/config.ts|]
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabled spec,
          "databaseUrlEnvVarName" .= Db.databaseUrlEnvVarName,
          "defaultClientUrl" .= WebApp.getDefaultDevClientUrl spec,
          "defaultServerUrl" .= Server.defaultDevServerUrl,
          "defaultServerPort" .= Server.defaultServerPort
        ]

genClientConfigFile :: Generator FileDraft
genClientConfigFile = return $ C.mkTmplFdWithData relConfigFilePath tmplData
  where
    relConfigFilePath = [relfile|client/config.ts|]
    tmplData = object ["defaultServerUrl" .= Server.defaultDevServerUrl]

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

depsRequiredForAuth :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredForAuth spec =
  [AS.Dependency.make ("@stitches/react", show versionRange) | isAuthEnabled spec]
  where
    versionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 1 2 8)]

depsRequiredByTailwind :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByTailwind spec =
  if G.CF.isTailwindUsed spec
    then
      AS.Dependency.fromList
        [ ("tailwindcss", "^3.2.7"),
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
      C.mkTmplFd [relfile|universal/validators.ts|]
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

  let tmplData = object ["areThereAnyEntitiesDefined" .= areThereAnyEntitiesDefined]

  return $
    C.mkTmplFdWithData
      [relfile|server/dbClient.ts|]
      tmplData
