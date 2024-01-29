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
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.ServerGenerator.AuthG as ServerAuthG
import Wasp.Generator.Templates (TemplatesDir, getTemplatesDirAbsPath)
import Wasp.Project.Common (WaspProjectDir)
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
            [ "depsChunk" .= N.getDependenciesPackageJsonEntry npmDeps,
              "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry npmDeps
            ]
      )
  where
    npmDeps = npmDepsForSdk spec

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
            ("jsonwebtoken", "^8.5.1"),
            ("mitt", "3.0.0"),
            ("react", "^18.2.0"),
            ("react-router-dom", "^5.3.3"),
            ("react-hook-form", "^7.45.4"),
            ("secure-password", "^4.0.0"),
            ("superjson", "^1.12.2"),
            ("@types/express-serve-static-core", "^4.17.13")
          ]
          ++ depsRequiredForAuth spec
          -- This must be installed in the SDK because it lists prisma/client as a dependency.
          -- Installing it inside .wasp/out/server/node_modules would also
          -- install prisma/client in the same folder, which would cause our
          -- runtime to load the wrong (uninitialized prisma/client)
          -- TODO(filip): Find a better way to handle duplicate
          -- dependencies: https://github.com/wasp-lang/wasp/issues/1640
          ++ ServerAuthG.depsRequiredByAuth spec,
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

-- TODO(filip): Figure out where this belongs. Check https://github.com/wasp-lang/wasp/pull/1602#discussion_r1437144166 .
-- Also, fix imports for wasp project.
installNpmDependencies :: Path' Abs (Dir WaspProjectDir) -> J.Job
installNpmDependencies projectDir =
  runNodeCommandAsJob projectDir "npm" ["install"] J.Wasp
