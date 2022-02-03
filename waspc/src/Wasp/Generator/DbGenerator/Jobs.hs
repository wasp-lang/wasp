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
import Wasp.Generator.Common (ProjectRootDir, prismaVersion)
import Wasp.Generator.DbGenerator.Common (dbSchemaFileInProjectRootDir)
import Wasp.Generator.Job (JobMessage, JobMessageData (JobExit, JobOutput))
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInProjectRootDir)

-- `--no-install` is the magic that causes this command to fail if npx cannot find it locally
--    (either in node_modules, or globally in npx). We do not want to allow npx to ask
--    a user without prisma to install the latest version.
-- We also version pin to what we need so it won't accidentally find a different version globally
--   somewhere on the PATH.
npxPrismaCmd :: [String]
npxPrismaCmd = ["npx", "--no-install", "prisma@" ++ prismaVersion]

migrateDev :: Path' Abs (Dir ProjectRootDir) -> J.Job
migrateDev projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  -- NOTE(matija): We are running this command from server's root dir since that is where
  -- Prisma packages (cli and client) are currently installed.
  -- NOTE(martin): `prisma migrate dev` refuses to execute when interactivity is needed if stdout is being piped,
  --   because it assumes it is used in non-interactive environment. In our case we are piping both stdin and stdout
  --   so we do have interactivity, but Prisma doesn't know that.
  --   I opened an issue with Prisma https://github.com/prisma/prisma/issues/7113, but in the meantime
  --   we are using `script` to trick Prisma into thinking it is running in TTY (interactively).

  -- NOTE(martin): For this to work on Mac, filepath in the list below must be as it is now - not wrapped in any quotes.
  let npxPrismaMigrateCmd = npxPrismaCmd ++ ["migrate", "dev", "--schema", SP.toFilePath schemaFile]
  let scriptArgs =
        if System.Info.os == "darwin"
          then -- NOTE(martin): On MacOS, command that `script` should execute is treated as multiple arguments.
            ["-Fq", "/dev/null"] ++ npxPrismaMigrateCmd
          else -- NOTE(martin): On Linux, command that `script` should execute is treated as one argument.
            ["-feqc", unwords npxPrismaMigrateCmd, "/dev/null"]

  let job = runNodeCommandAsJob serverDir "script" scriptArgs J.Db

  retryJobOnErrorWith job (npmInstall projectDir) allJobOutput

-- | Runs `prisma studio` - Prisma's db inspector.
runStudio :: Path' Abs (Dir ProjectRootDir) -> J.Job
runStudio projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  let npxPrismaStudioCmd = npxPrismaCmd ++ ["studio", "--schema", SP.toFilePath schemaFile]
  let job = runNodeCommandAsJob serverDir (head npxPrismaStudioCmd) (tail npxPrismaStudioCmd) J.Db

  retryJobOnErrorWith job (npmInstall projectDir) allJobOutput

generatePrismaClient :: Path' Abs (Dir ProjectRootDir) -> J.Job
generatePrismaClient projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  let npxPrismaGenerateCmd = npxPrismaCmd ++ ["generate", "--schema", SP.toFilePath schemaFile]
  let job = runNodeCommandAsJob serverDir (head npxPrismaGenerateCmd) (tail npxPrismaGenerateCmd) J.Db

  retryJobOnErrorWith job (npmInstall projectDir) silentUntilError

-- | Runs `npm install` to install dependencies.
-- May be needed before npx commands if dependencies (e.g. Prisma) are not installed.
npmInstall :: Path' Abs (Dir ProjectRootDir) -> J.Job
npmInstall projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  runNodeCommandAsJob serverDir "npm" ["install"] J.Db

-- | Runs the original job, and if there is an error, it attempts recovery by running the second job
-- before retrying the original one more time.
-- It also takes a function that handles forwarding channel output.
-- NOTE(shayne): We only want to forward one JobExit message in this function, as callers would stop
-- listening once they get it. This is why our job message forwarders do not send them and we do ourselves at the end.
retryJobOnErrorWith :: J.Job -> J.Job -> JobMessageForwarder -> J.Job
retryJobOnErrorWith originalJob afterErrorJob channelForwarder externalChannel = do
  internalChannel <- newChan
  (initialExitCode, _) <- concurrently (originalJob internalChannel) (channelForwarder internalChannel externalChannel)
  exitCode <-
    case initialExitCode of
      ExitSuccess -> return ExitSuccess
      ExitFailure _ -> do
        _ <- concurrently (afterErrorJob internalChannel) (channelForwarder internalChannel externalChannel)
        (retryExitCode, _) <- concurrently (originalJob internalChannel) (channelForwarder internalChannel externalChannel)
        return retryExitCode
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

type JobMessageForwarder = Chan JobMessage -> Chan JobMessage -> IO ()

-- | Forwards all JobOutput message to the output channel, but does not send JobExit
-- as multiple jobs may be forwarded to the same output channel. The caller is responsible
-- for sending a final JobExit.
allJobOutput :: JobMessageForwarder
allJobOutput fromChan toChan = do
  message <- readChan fromChan
  case J._data message of
    JobOutput _ _ -> writeChan toChan message >> allJobOutput fromChan toChan
    JobExit _ -> return ()

-- | Forwards all JobOutput message to the output channel on error, but does not send JobExit
-- as multiple jobs may be forwarded to the same output channel. The caller is responsible
-- for sending a final JobExit.
silentUntilError :: JobMessageForwarder
silentUntilError fromChan toChan = silentUntilError' []
  where
    silentUntilError' messagesSoFar = do
      newMessage <- readChan fromChan
      case J._data newMessage of
        JobOutput _ _ -> silentUntilError' (newMessage : messagesSoFar)
        JobExit exitCode -> case exitCode of
          ExitSuccess -> return ()
          ExitFailure _ -> writeList2Chan toChan messagesSoFar
