module Wasp.Cli.Command.Start
  ( start,
  )
where

import Control.Concurrent.Async (race)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
    waspSaysC,
  )
import Wasp.Cli.Command.Compile (compileIO)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Terminal (asWaspFailureMessage, asWaspStartMessage, asWaspSuccessMessage)
import qualified Wasp.Generator.Common
import qualified Wasp.Lib

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Command ()
start = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  compileAndNpmInstall waspRoot outDir

  -- waspSaysC $ asWaspStartMessage "Compiling wasp code..."
  -- compilationResult <- liftIO $ compileIO waspRoot outDir
  -- case compilationResult of
  --   Left compileError -> throwError $ CommandError $ asWaspFailureMessage "Compilation failed:" ++ compileError
  --   Right () -> waspSaysC $ asWaspSuccessMessage "Code has been successfully compiled, project has been generated."

  -- -- TODO: Do smart npm install -> if we need to install stuff, install it, otherwise don't.
  -- --   This should be responsibility of Generator, it should tell us how to install stuff.
  -- --   But who checks out if stuff needs to be installed at all? That should probably be
  -- --   Generator again. After installation, it should return some kind of data that describes that installation.
  -- --   Then, next time, we give it data we have about last installation, and it uses that
  -- --   to decide if installation needs to happen or not. If it happens, it returnes new data again.
  -- --   Right now we have setup/installation being called, but it has not support for being "smart" yet.
  -- waspSaysC $ asWaspStartMessage "Setting up generated project..."
  -- setupResult <- liftIO $ Wasp.Lib.setup outDir
  -- case setupResult of
  --   Left setupError -> throwError $ CommandError $ asWaspFailureMessage "Setup failed:" ++ setupError
  --   Right () -> waspSaysC $ asWaspSuccessMessage "Setup successful."

  waspSaysC $ asWaspStartMessage "Listening for file changes..."
  waspSaysC $ asWaspStartMessage "Starting up generated project..."
  watchOrStartResult <- liftIO $ race (watch waspRoot outDir) (Wasp.Lib.start outDir)
  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left startError -> throwError $ CommandError $ asWaspFailureMessage "Start failed:" ++ startError
      Right () -> error "This should never happen, start should never end but it did."

compileAndNpmInstall ::
  Path' Abs (Dir Common.WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.Common.ProjectRootDir) ->
  Command ()
compileAndNpmInstall waspRoot outDir = do
  waspSaysC $ asWaspStartMessage "Compiling wasp code..."
  compilationResult <- liftIO $ compileIO waspRoot outDir
  case compilationResult of
    Left compileError -> throwError $ CommandError $ asWaspFailureMessage "Compilation failed:" ++ compileError
    Right () -> waspSaysC $ asWaspSuccessMessage "Code has been successfully compiled, project has been generated."

  -- TODO: Do smart install -> if we need to install stuff, install it, otherwise don't.
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