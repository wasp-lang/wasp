module Generator.Start
       ( start
       ) where

import           Control.Concurrent              (Chan, newChan, readChan)
import           Control.Concurrent.Async        (race)

import           Generator.Common                (ProjectRootDir)
import           Generator.Job                   (JobMessage)
import           Generator.Job.IO                (printJobMessage)
import           Generator.ServerGenerator.Start (startServer)
import           Generator.WebAppGenerator.Start (startWebApp)
import           StrongPath                      (Abs, Dir, Path)


-- | This is a blocking action, that will start the processes that run web app and server.
--   It will wait for those processes to end, but since they are made to keep running until error
--   occurs, so will this action, run until one of them fails or it fails itself.
start :: Path Abs (Dir ProjectRootDir) -> IO (Either String ())
start projectDir = do
    chan <- newChan
    let runStartJobs = race (startServer projectDir chan) (startWebApp projectDir chan)
    result <- race (handleJobMessages chan) runStartJobs
    case result of
        Left () -> error "App start: Reading job messages stopped too early, this should never happen."
        Right serverOrWebExitCode -> case serverOrWebExitCode of
            Left serverExitCode -> return $ Left $ "Server failed with exit code " ++ show serverExitCode ++ "."
            Right webAppExitCode -> return $ Left $ "Web app failed with exit code " ++ show webAppExitCode ++ "."
  where
      handleJobMessages :: Chan JobMessage -> IO ()
      handleJobMessages chan = readChan chan >>= printJobMessage >> handleJobMessages chan
