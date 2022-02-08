module Wasp.Cli.Command.CompileAndSetup (compileAndSetup) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( waspSaysC,
  )
import Wasp.Cli.Command.Compile (compileIO)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Terminal (asWaspFailureMessage, asWaspStartMessage, asWaspSuccessMessage)
import qualified Wasp.Generator.Common
import qualified Wasp.Lib

compileAndSetup ::
  Path' Abs (Dir Common.WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.Common.ProjectRootDir) ->
  Command ()
compileAndSetup waspRoot outDir = do
  waspSaysC $ asWaspStartMessage "Compiling wasp code..."
  compilationResult <- liftIO $ compileIO waspRoot outDir
  case compilationResult of
    Left compileError -> throwError $ CommandError $ asWaspFailureMessage "Compilation failed:" ++ compileError
    Right () -> waspSaysC $ asWaspSuccessMessage "Code has been successfully compiled, project has been generated."

  -- TODO: Do smart npm install -> if we need to install stuff, install it, otherwise don't.
  --   This should be responsibility of Generator, it should tell us how to install stuff.
  --   But who checks out if stuff needs to be installed at all? That should probably be
  --   Generator again. After installation, it should return some kind of data that describes that installation.
  --   Then, next time, we give it data we have about last installation, and it uses that
  --   to decide if installation needs to happen or not. If it happens, it returnes new data again.
  --   Right now we have setup/installation being called, but it has not support for being "smart" yet.
  waspSaysC $ asWaspStartMessage "Setting up generated project..."
  setupResult <- liftIO $ Wasp.Lib.setup outDir
  case setupResult of
    Left setupError -> throwError $ CommandError $ asWaspFailureMessage "Setup failed:" ++ setupError
    Right () -> waspSaysC $ asWaspSuccessMessage "Setup successful."