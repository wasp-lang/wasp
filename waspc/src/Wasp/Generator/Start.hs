module Wasp.Generator.Start
  ( start,
  )
where

import Control.Concurrent (Chan)
import Control.Concurrent.Async (concurrently, race)
import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Job (JobMessage)
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Generator.ServerGenerator.Start (startServer)
import Wasp.Generator.WebAppGenerator.Start (startWebApp)

-- | This is a blocking action, that will start the processes that run web app and server.
--   It will run as long as one of those processes does not fail.
start :: Path' Abs (Dir ProjectRootDir) -> Chan JobMessage -> IO (Either String ())
start projectDir chan = do
  let runStartJobs = race (startServer projectDir chan) (startWebApp projectDir chan)
  (_, serverOrWebExitCode) <- concurrently (readJobMessagesAndPrintThemPrefixed chan) runStartJobs
  case serverOrWebExitCode of
    Left serverExitCode -> return $ Left $ "Server failed with exit code " ++ show serverExitCode ++ "."
    Right webAppExitCode -> return $ Left $ "Web app failed with exit code " ++ show webAppExitCode ++ "."
