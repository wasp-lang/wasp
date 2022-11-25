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
  errorOrMaybeFullStackDeps <- isNpmInstallNeeded spec dstDir
  case errorOrMaybeFullStackDeps of
    Left errorMessage -> return ([], [GenericGeneratorError errorMessage])
    Right maybeFullStackDeps -> do
      case maybeFullStackDeps of
        Nothing -> return ([], [])
        Just fullStackDeps -> do
          sendMessage $ Msg.Start "Starting npm install..."
          (Left (npmInstallWarnings, npmInstallErrors)) <-
            installNpmDependenciesWithInstallRecord fullStackDeps dstDir
              `race` reportInstallationProgress reportInstallationProgressMessages
          if null npmInstallErrors
            then do
              sendMessage $ Msg.Success "Successfully completed npm install."
              sendMessage $ Msg.Start "Setting up database..."
              (dbGeneratorWarnings, dbGeneratorErrors) <- DbGenerator.postWriteDbGeneratorActions spec dstDir
              when (null dbGeneratorErrors) (sendMessage $ Msg.Success "Database successfully set up.")
              return (npmInstallWarnings ++ dbGeneratorWarnings, dbGeneratorErrors)
            else do
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
        "Installation going great - we will get there soon!",
        "Installation is taking a bit longer, but we will get there!",
        "Yup, still not done installing.",
        "We are getting closer and closer, soon it will all be installed!",
        "You still waiting for installation to finish? You should! We got too far to give up now!",
        "You waited so patiently, wait just a bit more (for installation to finish)..."
      ]

    secToMicroSec = (* 1000000)

    hasLessThan2Elems = null . drop 1
