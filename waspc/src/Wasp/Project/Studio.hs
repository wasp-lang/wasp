module Wasp.Project.Studio
  ( startStudio,
  )
where

import System.Exit (ExitCode (..))
import Wasp.Job.Process (runInteractiveProcess)
import Wasp.NodePackageFFI (RunnablePackage (WaspStudioPackage), getPackageProcessOptions)

startStudio ::
  -- | Path to the data JSON file.
  FilePath ->
  -- | All arguments from the Wasp CLI.
  IO (Either String ())
startStudio pathToDataFile = do
  let startStudioArgs = ["--data-file", pathToDataFile]

  cp <- getPackageProcessOptions WaspStudioPackage startStudioArgs
  exitCode <- runInteractiveProcess cp
  case exitCode of
    ExitSuccess -> return $ Right ()
    ExitFailure code -> return $ Left $ "Studio command failed with exit code: " ++ show code
