module Wasp.Generator.Setup
  ( runSetup,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import Wasp.Generator.NpmInstall (ensureNpmInstall)
import qualified Wasp.Message as Msg

runSetup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runSetup spec dstDir sendMessage = do
  sendMessage $ Msg.Start "Starting npm install..."
  (npmInstallWarnings, npmInstallErrors) <- ensureNpmInstall spec dstDir
  if null npmInstallErrors
    then do
      sendMessage $ Msg.Success "Successfully completed npm install."
      sendMessage $ Msg.Start "Setting up database..."
      (dbGeneratorWarnings, dbGeneratorErrors) <- DbGenerator.postWriteDbGeneratorActions spec dstDir
      if null dbGeneratorErrors
        then sendMessage $ Msg.Success "Database successfully set up."
        else sendMessage $ Msg.Failure "Could not set up database."
      return (npmInstallWarnings ++ dbGeneratorWarnings, dbGeneratorErrors)
    else do
      sendMessage $ Msg.Failure "npm install failed!"
      return (npmInstallWarnings, npmInstallErrors)
