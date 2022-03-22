module Wasp.Generator.Setup
  ( runSetup,
  )
where

import Control.Monad (when)
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
      when (null dbGeneratorErrors) (sendMessage $ Msg.Success "Database successfully set up.")
      return (npmInstallWarnings ++ dbGeneratorWarnings, dbGeneratorErrors)
    else do
      return (npmInstallWarnings, npmInstallErrors)
