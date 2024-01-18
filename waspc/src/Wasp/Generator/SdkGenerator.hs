{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator
  ( genSdk,
    installNpmDependencies,
    genExternalCodeDir,
    sdkRootDirInProjectRootDir,
    buildSdk,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Data.Aeson (object)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types ((.=))
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import GHC.IO (unsafePerformIO)
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
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft, createCopyDirFileDraft, createTemplateFileDraft)
import qualified Wasp.Generator.FileDraft as FD
import Wasp.Generator.FileDraft.CopyDirFileDraft (CopyDirFileDraftDstDirStrategy (RemoveExistingDstDir))
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.Templates (TemplatesDir, getTemplatesDirAbsPath)
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (toLowerFirst, (<++>))

data SdkRootDir

data SdkTemplatesDir

genSdk :: AppSpec -> Generator [FileDraft]
genSdk spec =
  genSdkHardcoded
    <++> genSdkReal spec

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
    dstDir = projectRootDir </> sdkRootDirInProjectRootDir

genSdkReal :: AppSpec -> Generator [FileDraft]
genSdkReal spec =
  sequence
    [ genFileCopy [relfile|api/index.ts|],
      genFileCopy [relfile|api/events.ts|],
      genFileCopy [relfile|server/dbClient.ts|],
      genTsConfigJson,
      genServerUtils spec,
      genPackageJson spec
    ]
    <++> genUniversalDir
    <++> genExternalCodeDir (AS.externalCodeFiles spec)
    <++> genTypesAndEntitiesDirs spec
  where
    genFileCopy = return . mkTmplFd

genSdkHardcoded :: Generator [FileDraft]
genSdkHardcoded =
  return
    [ copyFolder [reldir|auth|],
      copyFolder [reldir|core|],
      copyFolder [reldir|ext-src|],
      copyFolder [reldir|operations|],
      copyFolder [reldir|rpc|],
      copyFolder [reldir|server/actions|],
      copyFolder [reldir|server/queries|],
      copyFolder [reldir|types|]
    ]
  where
    copyFolder :: Path' (Rel SdkTemplatesDir) (Dir d) -> FileDraft
    copyFolder modul =
      createCopyDirFileDraft
        RemoveExistingDstDir
        (dstFolder </> castRel modul)
        (srcFolder </> modul)
    dstFolder = sdkRootDirInProjectRootDir
    srcFolder = absSdkTemplatesDir
    absSdkTemplatesDir = unsafePerformIO getTemplatesDirAbsPath </> sdkTemplatesDirInTemplatesDir

genTypesAndEntitiesDirs :: AppSpec -> Generator [FileDraft]
genTypesAndEntitiesDirs spec =
  return
    [ entitiesIndexFileDraft,
      taggedEntitiesFileDraft,
      serializationFileDraft,
      typesIndexFileDraft
    ]
  where
    entitiesIndexFileDraft =
      mkTmplFdWithDstAndData
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
      mkTmplFdWithDstAndData
        [relfile|server/_types/taggedEntities.ts|]
        [relfile|server/_types/taggedEntities.ts|]
        (Just $ object ["entities" .= allEntities])
    serializationFileDraft =
      mkTmplFd
        [relfile|server/_types/serialization.ts|]
    typesIndexFileDraft =
      mkTmplFdWithDstAndData
        [relfile|server/_types/index.ts|]
        [relfile|server/_types/index.ts|]
        ( Just $
            object
              [ "entities" .= allEntities,
                "isAuthEnabled" .= isJust maybeUserEntityName,
                "userEntityName" .= userEntityName,
                "authEntityName" .= DbAuth.authEntityName,
                "authFieldOnUserEntityName" .= DbAuth.authFieldOnUserEntityName,
                "authIdentityEntityName" .= DbAuth.authIdentityEntityName,
                "identitiesFieldOnAuthEntityName" .= DbAuth.identitiesFieldOnAuthEntityName,
                "userFieldName" .= toLowerFirst userEntityName
              ]
        )
    userEntityName = fromMaybe "" maybeUserEntityName
    allEntities = map (makeJsonWithEntityData . fst) $ AS.getDecls @AS.Entity.Entity spec
    maybeUserEntityName = AS.refName . AS.App.Auth.userEntity <$> AS.App.auth (snd $ AS.Valid.getApp spec)

genPackageJson :: AppSpec -> Generator FileDraft
genPackageJson spec =
  return $
    mkTmplFdWithDstAndData
      [relfile|package.json|]
      [relfile|package.json|]
      ( Just $
          object
            [ "depsChunk" .= N.getDependenciesPackageJsonEntry npmDepsForSdk,
              "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry npmDepsForSdk
            ]
      )
  where
    npmDepsForSdk =
      N.NpmDepsForPackage
        { N.dependencies =
            AS.Dependency.fromList
              [ ("@prisma/client", show prismaVersion),
                ("prisma", show prismaVersion),
                ("@tanstack/react-query", "^4.29.0"),
                ("axios", "^1.4.0"),
                ("express", "~4.18.1"),
                ("jsonwebtoken", "^8.5.1"),
                ("mitt", "3.0.0"),
                ("react", "^18.2.0"),
                ("react-router-dom", "^5.3.3"),
                ("react-hook-form", "^7.45.4"),
                ("secure-password", "^4.0.0"),
                ("superjson", "^1.12.2"),
                ("@types/express-serve-static-core", "^4.17.13")
              ]
              ++ depsRequiredForAuth spec,
          N.devDependencies =
            AS.Dependency.fromList
              [ ("@tsconfig/node" <> majorNodeVersionStr, "latest")
              ]
        }
    majorNodeVersionStr = show (SV.major $ getLowestNodeVersionUserAllows spec)

-- todo(filip): remove this duplication, we have almost the same thing in the
-- ServerGenerator.
genTsConfigJson :: Generator FileDraft
genTsConfigJson = do
  return $
    mkTmplFdWithDstAndData
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

mkTmplFdWithDstAndData ::
  Path' (Rel SdkTemplatesDir) File' ->
  Path' (Rel SdkRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (sdkRootDirInProjectRootDir </> relDstPath)
    (sdkTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

mkTmplFdWithDst :: Path' (Rel SdkTemplatesDir) File' -> Path' (Rel SdkRootDir) File' -> FileDraft
mkTmplFdWithDst src dst = mkTmplFdWithDstAndData src dst Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData = mkTmplFdWithDstAndData relSrcPath relDstPath tmplData
  where
    relDstPath = castRel relSrcPath

mkTmplFd :: Path' (Rel SdkTemplatesDir) File' -> FileDraft
mkTmplFd path = mkTmplFdWithDst path (SP.castRel path)

sdkRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir SdkRootDir)
sdkRootDirInProjectRootDir = [reldir|sdk/wasp|]

sdkTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTemplatesDir)
sdkTemplatesDirInTemplatesDir = [reldir|sdk|]

extSrcDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir GeneratedExternalCodeDir)
extSrcDirInSdkRootDir = [reldir|ext-src|]

-- todo(filip): figure out where this belongs
-- also, fix imports for wasp project
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
    relDstPath = sdkRootDirInProjectRootDir </> extSrcDirInSdkRootDir </> SP.castRel (EC._pathInExtCodeDir file)
    absSrcPath = EC.fileAbsPath file

genSourceFile :: EC.CodeFile -> Generator FD.FileDraft
genSourceFile file = return $ FD.createTextFileDraft relDstPath text
  where
    filePathInSrcExtCodeDir = EC.filePathInExtCodeDir file
    text = EC.fileText file
    relDstPath = sdkRootDirInProjectRootDir </> extSrcDirInSdkRootDir </> SP.castRel filePathInSrcExtCodeDir

genUniversalDir :: Generator [FileDraft]
genUniversalDir =
  return
    [ mkTmplFd [relfile|universal/url.ts|],
      mkTmplFd [relfile|universal/types.ts|],
      mkTmplFd [relfile|universal/validators.js|]
    ]

genServerUtils :: AppSpec -> Generator FileDraft
genServerUtils spec = return $ mkTmplFdWithData [relfile|server/utils.ts|] (Just tmplData)
  where
    tmplData = object ["isAuthEnabled" .= (isAuthEnabled spec :: Bool)]