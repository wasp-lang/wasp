module Wasp.Generator.Test
  ( testWebApp,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Generator.WebAppGenerator.Test as WebAppTest
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: [String] -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
testWebApp args projectDir = do
  chan <- newChan
  let testWebAppJob = WebAppTest.testWebApp args projectDir chan
  (testExitCode, _) <-
    testWebAppJob `concurrently` readJobMessagesAndPrintThemPrefixed chan
  case testExitCode of
    ExitSuccess -> return $ Right ()
    -- Exit code 130 is thrown when user presses Ctrl+C.
    ExitFailure 130 -> return $ Right ()
    ExitFailure code -> return $ Left $ "Tests failed with exit code " ++ show code ++ "."
