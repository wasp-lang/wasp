module Wasp.Generator.Test
  ( testWebApp,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import qualified Wasp.Generator.WebAppGenerator.Test as WebAppTest
import qualified Wasp.Job as Job
import qualified Wasp.Job.Output as Output
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: [String] -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
testWebApp args waspProjectDir = do
  chan <- newChan
  let testWebAppJob = Job.runJob (WebAppTest.testWebApp args waspProjectDir) chan
  (testExitCode, _) <-
    testWebAppJob `concurrently` Output.printEventsPrefixedUntilExit chan
  case testExitCode of
    ExitSuccess -> return $ Right ()
    -- Exit code 130 is thrown when user presses Ctrl+C.
    ExitFailure 130 -> return $ Right ()
    ExitFailure code -> return $ Left $ "Tests failed with exit code " ++ show code ++ "."
