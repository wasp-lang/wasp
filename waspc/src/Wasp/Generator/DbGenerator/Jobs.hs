module Wasp.Generator.DbGenerator.Jobs
  ( migrateDev,
    runGenerate,
    runStudio,
  )
where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
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

prismaNpxCommand :: String
prismaNpxCommand = "prisma@" ++ prismaVersion

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
  let npxPrismaCmd = ["npx", "--no-install", prismaNpxCommand, "migrate", "dev", "--schema", SP.toFilePath schemaFile]
  let scriptArgs =
        if System.Info.os == "darwin"
          then -- NOTE(martin): On MacOS, command that `script` should execute is treated as multiple arguments.
            ["-Fq", "/dev/null"] ++ npxPrismaCmd
          else -- NOTE(martin): On Linux, command that `script` should execute is treated as one argument.
            ["-feqc", unwords npxPrismaCmd, "/dev/null"]

  let job = runNodeCommandAsJob serverDir "script" scriptArgs J.Db

  retryJobOnErrorWith job (npmInstall projectDir)

-- | Runs `prisma studio` - Prisma's db inspector.
runStudio :: Path' Abs (Dir ProjectRootDir) -> J.Job
runStudio projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  let job =
        runNodeCommandAsJob
          serverDir
          "npx"
          [ "--no-install",
            prismaNpxCommand,
            "studio",
            "--schema",
            SP.toFilePath schemaFile
          ]
          J.Db

  retryJobOnErrorWith job (npmInstall projectDir)

-- | Runs `prisma generate` to generate the Prisma client.
runGenerate :: Path' Abs (Dir ProjectRootDir) -> J.Job
runGenerate projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir
  let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

  let job =
        runNodeCommandAsJob
          serverDir
          "npx"
          [ "--no-install",
            prismaNpxCommand,
            "generate",
            "--schema",
            SP.toFilePath schemaFile
          ]
          J.Db

  retryJobOnErrorWith job (npmInstall projectDir)

-- | Runs `npm install` to install dependencies.
-- May be needed before npx commands if dependencies (e.g. Prisma) are not installed.
npmInstall :: Path' Abs (Dir ProjectRootDir) -> J.Job
npmInstall projectDir = do
  let serverDir = projectDir </> serverRootDirInProjectRootDir

  runNodeCommandAsJob
    serverDir
    "npm"
    ["install"]
    J.Db

-- | Runs the original job, and if there is an error, it attempts recovery by running the second job
-- before retrying the original one more time.
-- It also takes care of forwarding all job output, but skips the job exit messages until this function is complete.
retryJobOnErrorWith :: J.Job -> J.Job -> J.Job
retryJobOnErrorWith originalJob afterErrorJob externalChannel = do
  internalChannel <- newChan
  (initialExitCode, _) <- concurrently (originalJob internalChannel) (forwardJobOutput internalChannel externalChannel)
  exitCode <-
    case initialExitCode of
      ExitSuccess -> return ExitSuccess
      ExitFailure _ -> do
        _ <- concurrently (afterErrorJob internalChannel) (forwardJobOutput internalChannel externalChannel)
        (retryExitCode, _) <- concurrently (originalJob internalChannel) (forwardJobOutput internalChannel externalChannel)
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
    forwardJobOutput :: Chan JobMessage -> Chan JobMessage -> IO ()
    forwardJobOutput fromChan toChan = do
      message <- readChan fromChan
      case J._data message of
        JobOutput _ _ -> writeChan toChan message >> forwardJobOutput fromChan toChan
        JobExit _ -> return ()
