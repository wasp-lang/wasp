module Wasp.Cli.Command.Deploy
  ( deploy,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getExecutablePath)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import qualified Wasp.Project.Deployment

deploy :: [String] -> Command ()
deploy cmdArgs = do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  deployResult <- liftIO $ do
    -- `getExecutablePath` has some caveats:
    --   https://frasertweedale.github.io/blog-fp/posts/2022-05-10-improved-executable-path-queries.html
    -- Once we upgrade to GHC 9.4 we should change to `executablePath`, but this should be ok for
    -- our purposes.
    waspExePath <- getExecutablePath
    Wasp.Project.Deployment.deploy waspExePath waspProjectDir cmdArgs
  either (throwError . CommandError "Deploy command failed") return deployResult
