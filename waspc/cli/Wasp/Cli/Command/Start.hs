module Wasp.Cli.Command.Start
  ( start,
  )
where

import Control.Concurrent.Async (race)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
    waspSaysC,
  )
import Wasp.Cli.Command.Compile
  ( compileIO,
  )
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Terminal (asWaspFailureMessage, asWaspStartMessage, asWaspSuccessMessage)
import qualified Wasp.Lib

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Command ()
start = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  waspSaysC $ asWaspStartMessage "Starting compilation and setup phase. Hold tight..."
  compilationResult <- liftIO $ compileIO waspRoot outDir
  case compilationResult of
    Left compileError -> throwError $ CommandError $ asWaspFailureMessage "Compilation failed:" ++ compileError
    Right () -> waspSaysC $ asWaspSuccessMessage "Code has been successfully compiled, project has been generated."

  waspSaysC $ asWaspStartMessage "Listening for file changes..."
  waspSaysC $ asWaspStartMessage "Starting up generated project..."
  watchOrStartResult <- liftIO $ race (watch waspRoot outDir) (Wasp.Lib.start outDir)
  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left startError -> throwError $ CommandError $ asWaspFailureMessage "Start failed:" ++ startError
      Right () -> error "This should never happen, start should never end but it did."
