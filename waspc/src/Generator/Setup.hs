module Generator.Setup
       ( setup
       ) where

import           Control.Concurrent              (Chan, newChan, readChan)
import           Control.Concurrent.Async        (concurrently)
import           System.Exit                     (ExitCode (..))

import           Generator.Common                (ProjectRootDir)
import qualified Generator.Job                   as J
import           Generator.Job.IO                (printJobMessage)
import           Generator.ServerGenerator.Setup (setupServer)
import           Generator.WebAppGenerator.Setup (setupWebApp)
import           StrongPath                      (Abs, Dir, Path)


setup :: Path Abs (Dir ProjectRootDir) -> IO (Either String ())
setup projectDir = do
    chan <- newChan
    let runSetupJobs = concurrently (setupServer projectDir chan) (setupWebApp projectDir chan)
    (_, result) <- concurrently (handleJobMessages chan (False, False)) runSetupJobs
    case result of
        (ExitSuccess, ExitSuccess) -> return $ Right ()
        exitCodes -> return $ Left $ setupFailedMessage exitCodes
  where
      handleJobMessages :: Chan J.JobMessage -> (Bool, Bool) -> IO ()
      handleJobMessages _ (True, True) = return ()
      handleJobMessages chan (isWebAppDone, isServerDone) = do
          jobMsg <- readChan chan
          case J._data jobMsg of
              J.JobOutput {} -> printJobMessage jobMsg >> handleJobMessages chan (isWebAppDone, isServerDone)
              J.JobExit {} -> case J._jobType jobMsg of
                  J.WebApp -> handleJobMessages chan (True, isServerDone)
                  J.Server -> handleJobMessages chan (isWebAppDone, True)

      setupFailedMessage (serverExitCode, webAppExitCode) =
          let serverErrorMessage = case serverExitCode of
                  ExitFailure code -> " Server setup failed with exit code " ++ show code ++ "."
                  _ -> ""
              webAppErrorMessage = case webAppExitCode of
                  ExitFailure code -> " Web app setup failed with exit code " ++ show code ++ "."
                  _ -> ""
          in "Setup failed!" ++ serverErrorMessage ++ webAppErrorMessage
