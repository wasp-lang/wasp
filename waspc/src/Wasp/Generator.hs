module Wasp.Generator
  ( generateWebAppCode,
    writeWebAppCode,
    isGeneratedAppUpToDate,
    Wasp.Generator.Start.start,
    Wasp.Generator.Test.testWebApp,
    GeneratedAppDir,
  )
where

import Control.Arrow (left, second)
import Control.Monad (forM_, when)
import Control.Monad.Extra (andM)
import Data.List.NonEmpty (toList)
import StrongPath (Abs, Dir, Path')
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.DbGenerator (genDb, isPrismaClientUpToDate)
import Wasp.Generator.DockerGenerator (genDockerFiles)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad
  ( Generator,
    GeneratorError,
    GeneratorWarning (GenericGeneratorWarning),
    logGeneratorWarning,
    runGenerator,
  )
import Wasp.Generator.NpmInstall (isNpmInstallNeeded)
import Wasp.Generator.SdkGenerator (genSdk)
import Wasp.Generator.ServerGenerator (genServer)
import Wasp.Generator.Setup (runSetup)
import qualified Wasp.Generator.Start
import qualified Wasp.Generator.Test
import Wasp.Generator.TypeAugmentationGenerator (genTypeAugmentation)
import Wasp.Generator.Valid (validateExternalConfigsWithAppSpec)
import qualified Wasp.Generator.WaspInfo as WaspInfo
import Wasp.Generator.WaspLibs (genWaspLibs)
import Wasp.Generator.WriteFileDrafts (areFileDraftsSynchronizedWithDisk, synchronizeFileDraftsWithDisk)
import Wasp.Message (SendMessage)
import Wasp.Util ((<++>))

-- | Validates the app spec and generates the web app code in memory.
generateWebAppCode :: AppSpec -> ([GeneratorWarning], Either [GeneratorError] [FileDraft])
generateWebAppCode spec =
  case validateExternalConfigsWithAppSpec spec of
    validationErrors@(_ : _) -> ([], Left validationErrors)
    [] -> second (left toList) $ runGenerator $ genApp spec

-- | Writes generated web app code to the destination directory and sets it up.
--   If dstDir does not exist yet, it will be created.
--   If there are any errors returned, setup failed but new code was possibly still written.
--   If no errors were returned, the generated project was successfully set up.
--   NOTE(martin): What if there is already smth in the dstDir? It is probably best
--     if we clean it up first? But we don't want this to end up with us deleting stuff
--     from user's machine. Maybe we just overwrite and we are good?
writeWebAppCode :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> [FileDraft] -> SendMessage -> IO ([GeneratorWarning], [GeneratorError])
writeWebAppCode spec dstDir fileDrafts sendMessage = do
  -- We remove the previous `.waspinfo` before writing any files, so a concurrent
  -- freshness check can't pair the new checksum file with the previous build's
  -- completed setup.
  WaspInfo.remove dstDir
  synchronizeFileDraftsWithDisk dstDir fileDrafts
  WaspInfo.persist dstDir (AS.buildType spec) WaspInfo.SetupPending
  (setupGeneratorWarnings, setupGeneratorErrors) <- runSetup spec dstDir sendMessage
  when (null setupGeneratorErrors) $ WaspInfo.persist dstDir (AS.buildType spec) WaspInfo.SetupComplete
  return (setupGeneratorWarnings, setupGeneratorErrors)

-- | Returns 'True' if writing and setting up the generated app would change
-- nothing on disk.
isGeneratedAppUpToDate :: AppSpec -> Path' Abs (Dir GeneratedAppDir) -> [FileDraft] -> IO Bool
isGeneratedAppUpToDate spec dstDir fileDrafts =
  andM
    [ AS.buildType spec `WaspInfo.isCompleteBuildAt` dstDir,
      areFileDraftsSynchronizedWithDisk dstDir fileDrafts,
      not <$> isNpmInstallNeeded spec dstDir,
      isPrismaClientUpToDate spec dstDir
    ]

genApp :: AppSpec -> Generator [FileDraft]
genApp spec = do
  warnOverriddenDeps spec

  genServer spec
    <++> genSdk spec
    <++> genDb spec
    <++> genDockerFiles spec
    <++> genTypeAugmentation spec
    <++> genWaspLibs

warnOverriddenDeps :: AppSpec -> Generator ()
warnOverriddenDeps spec =
  forM_ overriddenDepNames $ \pkgName ->
    logGeneratorWarning $
      GenericGeneratorWarning $
        "Dependency override active for \""
          ++ pkgName
          ++ "\". You are using an unsupported version. "
          ++ "Wasp cannot guarantee compatibility."
  where
    overriddenDepNames = D.name <$> PJ.getOverriddenDeps (AS.packageJson spec)
