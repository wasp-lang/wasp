module Wasp.Generator.DbGenerator.Jobs
  ( migrateDev,
    generatePrismaClient,
    runStudio,
  )
where

import Control.Concurrent (Chan, newChan, readChan, writeChan, writeList2Chan)
import Control.Concurrent.Async (concurrently)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import qualified System.Info
import Wasp.Generator.Common (ProjectRootDir, buildNpmCmdWithArgs, buildNpxCmdWithArgs, prismaVersion)
import Wasp.Generator.DbGenerator.Common (dbSchemaFileInProjectRootDir)
import Wasp.Generator.Job (JobMessage, JobMessageData (JobExit, JobOutput))
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeDependentCommandAsJob)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInProjectRootDir)

-- Args to be passed to `npx` in order to run prisma.
-- `--no-install` is the magic that causes this command to fail if npx cannot find it locally
--    (either in node_modules, or globally in npx). We do not want to allow npx to ask
--    a user without prisma to install the latest version.
-- We also pin the version to what we need so it won't accidentally find a different version globally
--   somewhere on the PATH.
npxPrismaArgs :: [String]
npxPrismaArgs = ["--no-install", "prisma@" ++ show prismaVersion]

migrateDev :: Path' Abs (Dir ProjectRootDir) -> Maybe String -> J.Job
migrateDev projectDir maybeMigrationName = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  let optionalMigrationArgs = maybe [] (\name -> ["--name", name]) maybeMigrationName

  -- NOTE(martin): For this to work on Mac, filepath in the list below must be as it is now - not wrapped in any quotes.
  let npxPrismaMigrateArgs = npxPrismaArgs ++ ["migrate", "dev", "--schema", SP.toFilePath schemaFile] ++ optionalMigrationArgs
  let npxPrismaMigrateCmdWithArgs = buildNpxCmdWithArgs npxPrismaMigrateArgs

  -- NOTE(martin): `prisma migrate dev` refuses to execute when interactivity is needed if stdout is being piped,
  --   because it assumes it is used in non-interactive environment. In our case we are piping both stdin and stdout
  --   so we do have interactivity, but Prisma doesn't know that.
  --   I opened an issue with Prisma https://github.com/prisma/prisma/issues/7113, but in the meantime
  --   we are using `script` to trick Prisma into thinking it is running in TTY (interactively).
  let osSpecificCmdAndArgs = case System.Info.os of
        "darwin" -> osxCmdAndArgs
        "mingw32" -> winCmdAndArgs
        _ -> posixCmdAndArgs
        where
          osxCmdAndArgs =
            -- NOTE(martin): On MacOS, command that `script` should execute is treated as multiple arguments.
            ("script", ["-Fq", "/dev/null"] ++ uncurry (:) npxPrismaMigrateCmdWithArgs)
          posixCmdAndArgs =
            -- NOTE(martin): On Linux, command that `script` should execute is treated as one argument.
            ("script", ["-feqc", unwords $ uncurry (:) npxPrismaMigrateCmdWithArgs, "/dev/null"])
          winCmdAndArgs =
            -- TODO: For Windows we don't do anything at the moment.
            --   Does this work when interactivity is needed, or does Prisma block us same as on mac and linux?
            --   If it does, find an alternative to `script` since it does not exist for Windows.
            npxPrismaMigrateCmdWithArgs

  -- NOTE(matija): We are running this command from server's root dir since that is where
  --   Prisma packages (cli and client) are currently installed.
  let job = runNodeDependentCommandAsJob J.Db serverDir osSpecificCmdAndArgs

  retryJobOnErrorWith job (npmInstall projectDir) ForwardEverything

-- | Runs `prisma studio` - Prisma's db inspector.
runStudio :: Path' Abs (Dir ProjectRootDir) -> J.Job
runStudio projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  let npxPrismaStudioCmdWithArgs = buildNpxCmdWithArgs $ npxPrismaArgs ++ ["studio", "--schema", SP.toFilePath schemaFile]
  let job = runNodeDependentCommandAsJob J.Db serverDir npxPrismaStudioCmdWithArgs

  retryJobOnErrorWith job (npmInstall projectDir) ForwardEverything

generatePrismaClient :: Path' Abs (Dir ProjectRootDir) -> J.Job
generatePrismaClient projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  let npxPrismaGenerateCmdWithArgs = buildNpxCmdWithArgs $ npxPrismaArgs ++ ["generate", "--schema", SP.toFilePath schemaFile]
  let job = runNodeDependentCommandAsJob J.Db serverDir npxPrismaGenerateCmdWithArgs

  retryJobOnErrorWith job (npmInstall projectDir) ForwardOnlyRetryErrors

-- | Runs `npm install` to install dependencies.
-- May be needed before npx commands if dependencies (e.g. Prisma) are not installed.
npmInstall :: Path' Abs (Dir ProjectRootDir) -> J.Job
npmInstall projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  runNodeDependentCommandAsJob J.Db serverDir $ buildNpmCmdWithArgs ["install"]

data JobMessageForwardingStrategy = ForwardEverything | ForwardOnlyRetryErrors

data Attempt = InitialAttempt | Retry

-- | Runs the original job, and if there is an error, it attempts recovery by running the second job
-- before retrying the original one last time.
-- TODO(shayne): Consider moving this function and data structures into a separate module to highlight it
-- is unique. We could also decouple this from DB-only jobs by passing in _jobType or using one of the
-- messages as a clue.
retryJobOnErrorWith :: J.Job -> J.Job -> JobMessageForwardingStrategy -> J.Job
retryJobOnErrorWith originalJob afterErrorJob forwardingStrategy externalChannel = do
  let channelForwarder = case forwardingStrategy of
        ForwardEverything -> allJobOutput
        ForwardOnlyRetryErrors -> silentUntilRetryFails
  intermediateChannel <- newChan

  (initialExitCode, _) <- concurrently (originalJob intermediateChannel) (channelForwarder InitialAttempt intermediateChannel externalChannel)
  exitCode <-
    case initialExitCode of
      ExitSuccess -> return ExitSuccess
      ExitFailure _ -> do
        -- NOTE(shayne): We don't care what the exit code of the recovery job is. If it succeeds, great, and hopefully retry works.
        -- If it fails, retry will likely fail again in the same manner. There may be other quirks but this is
        -- sufficient for the npx and npm install use case we are targeting now.
        _ <- concurrently (afterErrorJob intermediateChannel) (channelForwarder InitialAttempt intermediateChannel externalChannel)
        (retryExitCode, _) <- concurrently (originalJob intermediateChannel) (channelForwarder Retry intermediateChannel externalChannel)
        return retryExitCode

  -- NOTE(shayne): We only want to forward one JobExit message to the external channel from this function, as callers will stop
  -- listening once they get it. This is why our job message forwarders do not send them and we do it ourselves here at the end.
  -- The reason is this function is creating a job that can run one or three sub-jobs, depending on success of the first. In the three sub-job
  -- case we have a first try (fails), a recovery job, and a final retry. Since JobExits indicate completion when sent over a channel, we cannot
  -- blindly forward all messages from the sub-jobs, as they would get 3 JobExits in this case (but stop listening on the first).
  -- Instead, we must ensure we only send one at the very end, when all sub-jobs are complete, and that it represents the ultimate success of the entire process.
  writeJobExitChanMessage exitCode
  return exitCode
  where
    writeJobExitChanMessage exitCode =
      writeChan
        externalChannel
        J.JobMessage
          { J._data = J.JobExit exitCode,
            J._jobType = J.Db
          }

    -- Forwards all JobOutput messages to the output channel, but does not send JobExit
    -- as this job might be only part of a bigger job, therefore we might not want to signal
    -- end of the job yet. The caller is responsible for sending a final JobExit.
    allJobOutput :: Attempt -> Chan JobMessage -> Chan JobMessage -> IO ()
    allJobOutput attempt fromChan toChan = do
      message <- readChan fromChan
      case J._data message of
        JobOutput _ _ -> writeChan toChan message >> allJobOutput attempt fromChan toChan
        JobExit _ -> return ()

    -- Only forwards JobOutput messages for the retry attempt if it fails.
    -- All first attempt output is ignored, and so is success on retry.
    -- We do not send JobExit as this job might be only part of a bigger job,
    -- therefore we might not want to signal end of the job yet.
    -- The caller is responsible for sending a final JobExit.
    silentUntilRetryFails :: Attempt -> Chan JobMessage -> Chan JobMessage -> IO ()
    silentUntilRetryFails attempt fromChan toChan = go []
      where
        go messagesSoFar = do
          newMessage <- readChan fromChan
          case J._data newMessage of
            JobOutput _ _ -> case attempt of
              InitialAttempt -> go []
              Retry -> go (newMessage : messagesSoFar)
            JobExit exitCode -> case exitCode of
              ExitSuccess -> return ()
              ExitFailure _ -> writeList2Chan toChan messagesSoFar
