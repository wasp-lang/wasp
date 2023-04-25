module Wasp.Cli.Command.Deploy
  ( deploy,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getExecutablePath)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import qualified Wasp.Project.Deployment

deploy :: [String] -> Command ()
deploy cmdArgs = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  deployResult <- liftIO $ do
    -- `getExecutablePath` has some caveats:
    --   https://frasertweedale.github.io/blog-fp/posts/2022-05-10-improved-executable-path-queries.html
    -- Once we upgrade to GHC 9.4 we should change to `executablePath`, but this should be ok for
    -- our purposes.
    waspExePath <- getExecutablePath
    Wasp.Project.Deployment.deploy waspExePath waspProjectDir cmdArgs
  case deployResult of
    Left err -> throwError $ CommandError "Deploy command failed" err
    Right () -> return ()
