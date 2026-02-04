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
import Data.Maybe (mapMaybe)
import StrongPath (Abs, Dir, Path', castRel, fromRelFile, (</>))
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.ExternalFiles as EF
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Common (ProjectRootDir)
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
import Wasp.Generator.SdkGenerator.Common
  ( extSrcDirInSdkRootDir,
    sdkRootDirInGeneratedCodeDir,
  )
import Wasp.Generator.SdkGenerator.Core (genCoreTsconfigProject)
import Wasp.Generator.SdkGenerator.Root (genRootTsconfigProject)
import Wasp.Generator.SdkGenerator.UserCore (genUserCoreTsconfigProject)
import Wasp.Generator.SdkGenerator.UserCore.EnvValidation (depsRequiredByEnvValidation)
import Wasp.Generator.SdkGenerator.UserCore.Server.EmailSenderG (depsRequiredByEmail)
import Wasp.Generator.SdkGenerator.UserCore.Server.JobGenerator (depsRequiredByJobs)
import Wasp.Generator.SdkGenerator.UserCore.Server.OAuthG (depsRequiredByOAuth)
import Wasp.Generator.SdkGenerator.UserCore.WebSocketGenerator (depsRequiredByWebSockets)
import qualified Wasp.Generator.ServerGenerator.AuthG as ServerAuthG
import Wasp.Generator.WaspLibs.AvailableLibs (waspLibs)
import Wasp.Generator.WaspLibs.Common (libsRootDirFromSdkDir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common (WaspProjectDir)
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
    sdkRootDir = projectRootDir </> sdkRootDirInGeneratedCodeDir

genSdk :: AppSpec -> Generator [FileDraft]
genSdk spec =
  genRootTsconfigProject spec (npmDepsForSdk spec)
    <++> genCoreTsconfigProject
    <++> genUserCoreTsconfigProject spec
    <++> genExternalCodeDir (AS.externalCodeFiles spec)

-- TODO(filip): Figure out where this belongs.
-- Check https://github.com/wasp-lang/wasp/pull/1602#discussion_r1437144166 .
-- Also, fix imports for wasp project.
installNpmDependencies :: Path' Abs (Dir WaspProjectDir) -> J.Job
installNpmDependencies projectDir =
  runNodeCommandAsJob projectDir "npm" ["install"] J.Wasp

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
      sdkRootDirInGeneratedCodeDir
        </> extSrcDirInSdkRootDir
        </> castRel externalFilePath

    externalFilePath = EF.filePathInExtCodeDir file

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
