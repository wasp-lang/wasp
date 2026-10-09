module Wasp.Generator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Generator.WebAppGenerator.Test as WebAppTest
import qualified Wasp.Job as Job
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util (exitCodeToEither)

testWebApp :: WebAppRunConfig -> [String] -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
testWebApp webAppRunConfig args waspProjectDir = do
  testExitCode <-
    Job.run
      $ Job.prefixWith Job.WebApp
      $ WebAppTest.testWebApp webAppRunConfig args waspProjectDir
  return $ case testExitCode of
    -- Exit code 130 is thrown when user presses Ctrl+C.
    ExitFailure 130 -> Right ()
    _ -> exitCodeToEither "Tests" testExitCode
