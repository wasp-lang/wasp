module Wasp.Cli.Command.Dockerfile
  ( printDockerfile,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as T.IO
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (defaultCompileOptions)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Message as Msg
import Wasp.Project (compileAndRenderDockerfile)

printDockerfile :: Command ()
printDockerfile = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  dockerfileContentOrCompileErrors <- liftIO $ compileAndRenderDockerfile waspProjectDir (defaultCompileOptions waspProjectDir)
  either
    (\errors -> liftIO (cliSendMessage $ Msg.Failure $ Msg.Entry "Displaying Dockerfile failed due to a compilation error in your Wasp project" [] : errors) >> throwError CommandAborted)
    (liftIO . T.IO.putStrLn)
    dockerfileContentOrCompileErrors
