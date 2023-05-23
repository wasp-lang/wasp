{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Cli.Command
  ( Command,
    runCommand,
    CommandError (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import System.Exit (exitFailure)
import Wasp.Cli.Command.Requires (MonadRequires, RequiresT, runRequiresT)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Message as Msg

newtype Command a = Command {_runCommand :: RequiresT (ExceptT CommandError IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CommandError, MonadRequires)

runCommand :: Command a -> IO ()
runCommand cmd = do
  runExceptT (runRequiresT $ _runCommand cmd) >>= \case
    Left cmdError -> do
      cliSendMessage $ Msg.Failure (_errorTitle cmdError) (_errorMsg cmdError)
      exitFailure
    Right _ -> return ()

-- TODO: What if we want to recognize errors in order to handle them?
--   Should we add _commandErrorType? Should CommandError be parametrized by it, is that even possible?
data CommandError = CommandError {_errorTitle :: !String, _errorMsg :: !String}
