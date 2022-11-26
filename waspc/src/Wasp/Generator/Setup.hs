module Wasp.Generator.Setup
  ( runSetup,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (when)
import StrongPath (Abs, Dir, Path')
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import Wasp.Generator.NpmInstall (installNpmDependenciesWithInstallRecord, isNpmInstallNeeded)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

runSetup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runSetup spec dstDir sendMessage = do
  ensureNpmInstall spec dstDir sendMessage >>= \case
    npmInstallResults@(_, []) -> (npmInstallResults <>) <$> setUpDatabase spec dstDir sendMessage
    npmInstallResults -> return npmInstallResults

ensureNpmInstall :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
ensureNpmInstall spec dstDir sendMessage = do
  isNpmInstallNeeded spec dstDir >>= \case
    Left errorMessage -> return ([], [GenericGeneratorError errorMessage])
    Right maybeFullStackDeps -> case maybeFullStackDeps of
      Nothing -> return ([], [])
      Just fullStackDeps -> do
        sendMessage $ Msg.Start "Starting npm install..."
        (Left (npmInstallWarnings, npmInstallErrors)) <-
          installNpmDependenciesWithInstallRecord fullStackDeps dstDir
            `race` reportInstallationProgress reportInstallationProgressMessages
        when (null npmInstallErrors) (sendMessage $ Msg.Success "Successfully completed npm install.")
        return (npmInstallWarnings, npmInstallErrors)
  where
    reportInstallationProgress :: [String] -> IO ()
    reportInstallationProgress messages = do
      threadDelay $ secToMicroSec 5
      putStrLn $ Term.applyStyles [Term.Yellow] $ "\n\n  ..." ++ head messages
      threadDelay $ secToMicroSec 5
      reportInstallationProgress $ if hasLessThan2Elems messages then messages else drop 1 messages

    reportInstallationProgressMessages =
      [ "Still installing npm dependencies!",
        "Installation going great - we'll get there soon!",
        "The installation is taking a while, but we'll get there!",
        "Yup, still not done installing.",
        "We're getting closer and closer, everything will be installed soon!",
        "Still waiting for the installation to finish? You should! We got too far to give up now!",
        "You've been waiting so patiently, just wait a little longer (for the installation to finish)..."
      ]

    secToMicroSec = (* 1000000)

    hasLessThan2Elems = null . drop 1

setUpDatabase :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
setUpDatabase spec dstDir sendMessage = do
  sendMessage $ Msg.Start "Setting up database..."
  (dbGeneratorWarnings, dbGeneratorErrors) <- DbGenerator.postWriteDbGeneratorActions spec dstDir
  when (null dbGeneratorErrors) (sendMessage $ Msg.Success "Database successfully set up.")
  return (dbGeneratorWarnings, dbGeneratorErrors)
