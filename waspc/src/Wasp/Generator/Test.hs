module Wasp.Generator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Generator.WebAppGenerator.Test as WebAppTest
import qualified Wasp.Job.Output as Output
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: WebAppRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
testWebApp webAppRunConfig args waspProjectDir = do
  testExitCode <- Output.runAndPrintPrefixedOutput $ WebAppTest.testWebApp webAppRunConfig args waspProjectDir
  case testExitCode of
    ExitSuccess -> return $ Right ()
    -- Exit code 130 is thrown when user presses Ctrl+C.
    ExitFailure 130 -> return $ Right ()
    ExitFailure code -> return $ Left $ "Tests failed with exit code " ++ show code ++ "."
