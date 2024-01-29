module Wasp.Generator.SdkGenerator where

import Data.Aeson (object)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types ((.=))
import GHC.IO (unsafePerformIO)
import StrongPath
import Wasp.AppSpec
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.Common (ProjectRootDir, prismaVersion)
import Wasp.Generator.FileDraft (FileDraft, createCopyDirFileDraft, createTemplateFileDraft)
import Wasp.Generator.FileDraft.CopyDirFileDraft (CopyDirFileDraftDstDirStrategy (RemoveExistingDstDir))
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.Templates (TemplatesDir, getTemplatesDirAbsPath)
import qualified Wasp.SemanticVersion as SV

genSdk :: AppSpec -> Generator [FileDraft]
genSdk spec = sequence [genSdkModules, genPackageJson spec]

data SdkRootDir

data SdkTemplatesDir

genSdkModules :: Generator FileDraft
genSdkModules =
  return $
    createCopyDirFileDraft
      RemoveExistingDstDir
      sdkRootDirInProjectRootDir
      (unsafePerformIO getTemplatesDirAbsPath </> sdkTemplatesDirInTemplatesDir </> [reldir|wasp|])

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

sdkRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir SdkRootDir)
sdkRootDirInProjectRootDir = [reldir|sdk/wasp|]

sdkTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTemplatesDir)
sdkTemplatesDirInTemplatesDir = [reldir|sdk|]
