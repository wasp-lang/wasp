module Wasp.Generator.Test
  ( testWebApp,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.Generator.WebAppGenerator.RunConfig (ClientRunConfig)
import qualified Wasp.Generator.WebAppGenerator.Test as WebAppTest
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: ClientRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
testWebApp clientRunConfig args waspProjectDir = do
  chan <- newChan
  let testWebAppJob = WebAppTest.testWebApp clientRunConfig args waspProjectDir chan
  (testExitCode, _) <-
    testWebAppJob `concurrently` readJobMessagesAndPrintThemPrefixed chan
  case testExitCode of
    ExitSuccess -> return $ Right ()
    -- Exit code 130 is thrown when user presses Ctrl+C.
    ExitFailure 130 -> return $ Right ()
    ExitFailure code -> return $ Left $ "Tests failed with exit code " ++ show code ++ "."
