module Wasp.Cli.Command.Start
  ( start,
  )
where

import Brick (Widget, simpleMain, str)
import Control.Concurrent.Async (race)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..), runCommand)
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
  )
import Wasp.Cli.Command.Compile
  ( compileIO,
  )
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Lib
import qualified Wasp.Message as Msg

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
-- start :: Command ()
-- start = do
--   waspRoot <- findWaspProjectRootDirFromCwd
--   let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

--   cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."
--   compilationResult <- liftIO $ compileIO waspRoot outDir
--   case compilationResult of
--     Left compileError -> throwError $ CommandError "Compilation failed" compileError
--     Right () -> cliSendMessageC $ Msg.Success "Code has been successfully compiled, project has been generated."

--   cliSendMessageC $ Msg.Start "Listening for file changes..."
--   cliSendMessageC $ Msg.Start "Starting up generated project..."
--   watchOrStartResult <- liftIO $ race (watch waspRoot outDir) (Wasp.Lib.start outDir)
--   case watchOrStartResult of
--     Left () -> error "This should never happen, listening for file changes should never end but it did."
--     Right startResult -> case startResult of
--       Left startError -> throwError $ CommandError "Start failed" startError
--       Right () -> error "This should never happen, start should never end but it did."

buildInterface :: Widget ()
buildInterface = do
  str "Starting compilation and setup phase. Hold tight..."

start :: Command ()
start = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir
  compRes <- liftIO $ compileIO waspRoot outDir
  case compRes of
    Left e -> liftIO $ simpleMain (str e :: Widget ())
    Right () -> liftIO $ simpleMain buildInterface
