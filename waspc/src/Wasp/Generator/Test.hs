module Wasp.Generator.Test
  ( testWebApp,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Project.WebApp.Test as WebAppTest

testWebApp :: [String] -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
testWebApp args waspProjectDir = do
  chan <- newChan
  let testWebAppJob = WebAppTest.testWebApp args waspProjectDir chan
  (testExitCode, _) <-
    testWebAppJob `concurrently` readJobMessagesAndPrintThemPrefixed chan
  case testExitCode of
    ExitSuccess -> return $ Right ()
    -- Exit code 130 is thrown when user presses Ctrl+C.
    ExitFailure 130 -> return $ Right ()
    ExitFailure code -> return $ Left $ "Tests failed with exit code " ++ show code ++ "."
