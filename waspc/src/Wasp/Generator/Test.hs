module Wasp.Generator.Test
  ( testWebApp,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Generator.WebAppGenerator.Test as WebAppTest

testWebApp :: [String] -> Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
testWebApp args projectDir = do
  chan <- newChan
  let testWebAppJob = WebAppTest.testWebApp args projectDir chan
  (testExitCode, _) <-
    testWebAppJob `concurrently` readJobMessagesAndPrintThemPrefixed chan
  case testExitCode of
    ExitSuccess -> return $ Right ()
    ExitFailure code -> return $ Left $ "Tests failed with exit code " ++ show code ++ "."
