module Wasp.Cli.Command.Deploy
  ( deploy,
    parseDeploy,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as O
import System.Environment (getExecutablePath)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Call (Call (Deploy))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import qualified Wasp.Project.Deployment

deployRestArgs :: O.Parser String
deployRestArgs =
  O.strArgument
    ( O.metavar "DEPLOY_ARGUMENTS"
        <> O.help "Currently only support fly.io. See https://wasp-lang.dev/docs/deploying."
    )

parseDeploy :: O.Parser Call
parseDeploy = Deploy <$> O.some deployRestArgs

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
  either (throwError . CommandError "Deploy command failed") return deployResult
