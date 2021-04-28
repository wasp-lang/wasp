module Generator.Start
  ( start,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently, race)
import Generator.Common (ProjectRootDir)
import Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Generator.ServerGenerator.Start (startServer)
import Generator.WebAppGenerator.Start (startWebApp)
import StrongPath (Abs, Dir, Path)

-- | This is a blocking action, that will start the processes that run web app and server.
--   It will run as long as one of those processes does not fail.
start :: Path Abs (Dir ProjectRootDir) -> IO (Either String ())
start projectDir = do
  chan <- newChan
  let runStartJobs = race (startServer projectDir chan) (startWebApp projectDir chan)
  (_, serverOrWebExitCode) <- concurrently (readJobMessagesAndPrintThemPrefixed chan) runStartJobs
  case serverOrWebExitCode of
    Left serverExitCode -> return $ Left $ "Server failed with exit code " ++ show serverExitCode ++ "."
    Right webAppExitCode -> return $ Left $ "Web app failed with exit code " ++ show webAppExitCode ++ "."
