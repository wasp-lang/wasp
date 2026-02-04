module Wasp.Generator
  ( writeWebAppCode,
    Wasp.Generator.Start.start,
    Wasp.Generator.Test.testWebApp,
    ProjectRootDir,
  )
where

import Control.Monad (forM_)
import Data.List.NonEmpty (toList)
import StrongPath (Abs, Dir, Path')
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator (genDb)
import Wasp.Generator.DockerGenerator (genDockerFiles)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad
  ( Generator,
    GeneratorError,
    GeneratorWarning (GenericGeneratorWarning),
    logGeneratorWarning,
    runGenerator,
  )
import Wasp.Generator.SdkGenerator (genSdk)
import Wasp.Generator.ServerGenerator (genServer)
import Wasp.Generator.Setup (runSetup)
import qualified Wasp.Generator.Start
import qualified Wasp.Generator.Test
import Wasp.Generator.Valid (validateAppSpec)
import qualified Wasp.Generator.WaspInfo as WaspInfo
import Wasp.Generator.WaspLibs (genWaspLibs)
import Wasp.Generator.WriteFileDrafts (synchronizeFileDraftsWithDisk)
import Wasp.Message (SendMessage)
import Wasp.Util ((<++>))

-- | Generates web app code from given Wasp and writes it to given destination directory.
--   If dstDir does not exist yet, it will be created.
--   If there are any errors returned, that means that generator failed but new code was possibly still written.
--   If no errors were returned, this means generator was successful and generated a new version of the project
--     (regardless of the warnings returned).
--   NOTE(martin): What if there is already smth in the dstDir? It is probably best
--     if we clean it up first? But we don't want this to end up with us deleting stuff
--     from user's machine. Maybe we just overwrite and we are good?
writeWebAppCode :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> SendMessage -> IO ([GeneratorWarning], [GeneratorError])
writeWebAppCode spec dstDir sendMessage = do
  case validateAppSpec spec of
    validationErrors@(_ : _) -> return ([], validationErrors)
    [] -> do
      let (generatorWarnings, generatorResult) = runGenerator $ genApp spec

      case generatorResult of
        Left generatorErrors -> return (generatorWarnings, toList generatorErrors)
        Right fileDrafts -> do
          synchronizeFileDraftsWithDisk dstDir fileDrafts
          WaspInfo.persist dstDir $ AS.buildType spec
          (setupGeneratorWarnings, setupGeneratorErrors) <- runSetup spec dstDir sendMessage
          return (generatorWarnings ++ setupGeneratorWarnings, setupGeneratorErrors)

genApp :: AppSpec -> Generator [FileDraft]
genApp spec = do
  warnOverriddenDeps spec

  genServer spec
    <++> genSdk spec
    <++> genDb spec
    <++> genDockerFiles spec
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
