module Wasp.Cli.Command.Db.Reset
  ( reset,
  )
where

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator.Common (ResetArgs (..), defaultResetArgs)
import Wasp.Generator.DbGenerator.Operations (dbReset)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

reset :: [String] -> Command ()
reset optionalResetArgs = do
  InWaspProject waspProjectDir <- require
  let genProjectDir =
        waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedCodeDirInDotWaspDir

  resetDatabase optionalResetArgs genProjectDir

resetDatabase :: [String] -> Path' Abs (Dir ProjectRootDir) -> Command ()
resetDatabase optionalResetArgs genProjectDir = do
  cliSendMessageC $ Msg.Start "Resetting the database..."
  -- liftIO tryMigrate >>= \case
  liftIO tryReset >>= \case
    Left errorMsg -> throwError $ CommandError "Database reset failed" errorMsg
    Right () -> cliSendMessageC $ Msg.Success "Database reset successful."
  where
    tryReset = runExceptT $ do
      resetArgs <- liftEither $ parseResetArgs optionalResetArgs
      ExceptT $ dbReset genProjectDir resetArgs

parseResetArgs :: [String] -> Either String ResetArgs
parseResetArgs resetArgs = do
  go resetArgs defaultResetArgs
  where
    go :: [String] -> ResetArgs -> Either String ResetArgs
    go [] rArgs = Right rArgs
    go ("--force" : rest) rArgs = go rest $ rArgs {_force = True}
    go unknown _ = Left $ "Unknown reset arg(s): " ++ unwords unknown
