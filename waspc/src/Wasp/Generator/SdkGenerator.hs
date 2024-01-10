{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator
  ( genSdk,
    installNpmDependencies,
  )
where

import Data.Aeson (object)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types ((.=))
import Data.Maybe (fromMaybe, isJust)
import GHC.IO (unsafePerformIO)
import StrongPath
import qualified StrongPath as SP
import Wasp.AppSpec
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.AppSpec.Valid (isAuthEnabled)
import qualified Wasp.AppSpec.Valid as AS.Valid
import Wasp.Generator.Common (ProjectRootDir, makeJsonWithEntityData, prismaVersion)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft, createCopyDirFileDraft, createTemplateFileDraft)
import Wasp.Generator.FileDraft.CopyDirFileDraft (CopyDirFileDraftDstDirStrategy (RemoveExistingDstDir))
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.Templates (TemplatesDir, getTemplatesDirAbsPath)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (toLowerFirst, (<++>))

genSdk :: AppSpec -> Generator [FileDraft]
genSdk spec = (:) <$> genPackageJson spec <*> genHardcodedSdkModules <++> genSdkModules spec

data SdkRootDir

data SdkTemplatesDir

genSdkModules :: AppSpec -> Generator [FileDraft]
genSdkModules spec =
  sequence
    [ genFileCopy [relfile|api/index.ts|],
      genFileCopy [relfile|api/events.ts|]
    ]
    <++> genTypesAndEntitiesDirs spec
  where
    genFileCopy = return . mkTmplFd

genHardcodedSdkModules :: Generator [FileDraft]
genHardcodedSdkModules =
  return
    [ copyFolder [reldir|auth|],
      copyFolder [reldir|core|],
      copyFolder [reldir|ext-src|],
      copyFolder [reldir|operations|],
      copyFolder [reldir|rpc|],
      copyFolder [reldir|server/actions|],
      copyFolder [reldir|server/queries|],
      copyFile [relfile|server/dbClient.ts|],
      copyFile [relfile|server/utils.ts|],
      copyFolder [reldir|types|],
      copyFolder [reldir|universal|]
    ]
  where
    copyFolder :: Path' (Rel SdkTemplatesDir) (Dir d) -> FileDraft
    copyFolder modul =
      createCopyDirFileDraft
        RemoveExistingDstDir
        (dstFolder </> castRel modul)
        (srcFolder </> modul)
    copyFile :: Path' (Rel SdkTemplatesDir) File' -> FileDraft
    copyFile = mkTmplFd
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
          N.devDependencies = AS.Dependency.fromList []
        }

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

mkTmplFd :: Path' (Rel SdkTemplatesDir) File' -> FileDraft
mkTmplFd path = mkTmplFdWithDst path (SP.castRel path)

sdkRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir SdkRootDir)
sdkRootDirInProjectRootDir = [reldir|sdk/wasp|]

sdkTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTemplatesDir)
sdkTemplatesDirInTemplatesDir = [reldir|sdk|]

-- todo(filip): figure out where this belongs
-- also, fix imports for wasp project
installNpmDependencies :: Path' Abs (Dir WaspProjectDir) -> J.Job
installNpmDependencies projectDir =
  runNodeCommandAsJob projectDir "npm" ["install"] J.Wasp
