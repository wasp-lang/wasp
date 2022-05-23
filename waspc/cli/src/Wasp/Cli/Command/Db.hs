{-# LANGUAGE LambdaCase #-}

module Wasp.Cli.Command.Db
  ( runDbCommand,
    studio,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, runCommand)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Compile (compileWithOptions, defaultCompileOptions)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import Wasp.CompileOptions (CompileOptions (warningsFilter))
import Wasp.Generator.DbGenerator.Jobs (runStudio)
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))
import qualified Wasp.Message as Msg

runDbCommand :: Command a -> IO ()
runDbCommand = runCommand . makeDbCommand

-- | This function makes sure that all the prerequisites which db commands
--   need are set up (e.g. makes sure Prisma CLI is installed).
--
--   All the commands that operate on db should be created using this function.
makeDbCommand :: Command a -> Command a
makeDbCommand cmd = do
  -- Ensure code is generated and npm dependencies are installed.
  waspProjectDir <- findWaspProjectRootDirFromCwd
  compileWithOptions $ compileOptions waspProjectDir
  cmd
  where
    compileOptions waspProjectDir =
      (defaultCompileOptions waspProjectDir)
        { warningsFilter =
            filter
              ( \case
                  GeneratorNeedsMigrationWarning _ -> False
                  _ -> True
              )
        }

-- TODO(matija): should we extract this into a separate file, like we did for migrate?
studio :: Command ()
studio = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let genProjectDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Running studio..."
  chan <- liftIO newChan

  _ <- liftIO $ concurrently (readJobMessagesAndPrintThemPrefixed chan) (runStudio genProjectDir chan)
  error "This should never happen, studio should never stop."
