module Wasp.Generator.Start
  ( start,
  )
where

import Control.Concurrent (dupChan, forkIO, newChan, readChan)
import Control.Concurrent.Async (concurrently, race)
import Control.Monad (forever)
import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Generator.ServerGenerator.Start (startServer)
import Wasp.Generator.WebAppGenerator.Start (startWebApp)

-- | This is a blocking action, that will start the processes that run web app and server.
--   It will run as long as one of those processes does not fail.
start :: Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
start projectDir = do
  chan <- newChan
  -- TODO: Next time we will take this into a separate function,
  -- we can store the contents in memory, we can overwrite the file each time
  -- we can add a HTML shell with JS to refresh, we can separate the
  -- output stream by _jobType (server, react, db, and wasp messages).
  -- Figure out where to write and how to automatically open.
  -- How to handle stopping app? Include last write timestamp (JS can check to warn).
  -- Can have a race between readChan and a timer to ensure we always write at least every x seconds.
  -- Have separate areas for warning/errors that we have seen.
  -- Can show some sort of app structure diagram, etc. (May require AppSpec)
  childChan <- dupChan chan
  _ <- forkIO $
    forever $ do
      contents <- readChan childChan
      appendFile "/tmp/test.html" (show contents)
  let runStartJobs = race (startServer projectDir chan) (startWebApp projectDir chan)
  (_, serverOrWebExitCode) <- concurrently (readJobMessagesAndPrintThemPrefixed chan) runStartJobs
  case serverOrWebExitCode of
    Left serverExitCode -> return $ Left $ "Server failed with exit code " ++ show serverExitCode ++ "."
    Right webAppExitCode -> return $ Left $ "Web app failed with exit code " ++ show webAppExitCode ++ "."
