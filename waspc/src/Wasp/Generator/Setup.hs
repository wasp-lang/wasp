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
import Wasp.Generator.NpmInstall (installNpmDependenciesWithInstallRecord)
import qualified Wasp.Message as Msg

runSetup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
runSetup spec dstDir sendMessage = do
  installNpmDependenciesWithInstallRecord spec dstDir >>= \case
    warnsAndErrors@(_, _errors@[]) -> do
      sendMessage $ Msg.Success "Successfully completed npm install."
      (warnsAndErrors <>) <$> setUpDatabase spec dstDir sendMessage
    warnsAndErrors -> return warnsAndErrors

setUpDatabase :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> Msg.SendMessage -> IO ([GeneratorWarning], [GeneratorError])
setUpDatabase spec dstDir sendMessage = do
  sendMessage $ Msg.Start "Setting up database..."
  (dbGeneratorWarnings, dbGeneratorErrors) <- DbGenerator.postWriteDbGeneratorActions spec dstDir
  when (null dbGeneratorErrors) (sendMessage $ Msg.Success "Database successfully set up.")
  return (dbGeneratorWarnings, dbGeneratorErrors)
