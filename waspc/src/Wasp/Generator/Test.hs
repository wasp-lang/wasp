module Wasp.Generator.Test
  ( testWebApp,
  )
where

import Data.Bifunctor (first)
import StrongPath (Abs, Dir, Path')
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Generator.WebAppGenerator.Test as WebAppTest
import qualified Wasp.Job as Job
import qualified Wasp.Job.Output as Output
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: WebAppRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
testWebApp webAppRunConfig args waspProjectDir = do
  testResult <- Output.withPrefixed (`Job.runJob` WebAppTest.testWebApp webAppRunConfig args waspProjectDir)
  case first Job.jobFailureExitCode testResult of
    Right () -> return $ Right ()
    -- Exit code 130 is thrown when user presses Ctrl+C.
    Left 130 -> return $ Right ()
    Left code -> return $ Left $ "Tests failed with exit code " ++ show code ++ "."
