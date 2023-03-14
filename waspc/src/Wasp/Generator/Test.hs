module Wasp.Generator.Test
  ( test,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Generator.WebAppGenerator.Test (testWebApp)

test :: Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
test projectDir = do
  chan <- newChan
  -- TODO: Add server tests in future.
  let runTestJobs = testWebApp projectDir chan
  (testExitCode, _) <-
    runTestJobs `concurrently` readJobMessagesAndPrintThemPrefixed chan
  case testExitCode of
    ExitSuccess -> return $ Right ()
    ExitFailure code -> return $ Left $ "Tests failed with exit code " ++ show code ++ "."
