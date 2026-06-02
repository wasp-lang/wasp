module Wasp.Cli.Command.Dockerfile
  ( printDockerfile,
    parserInfo,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as T.IO
import qualified Options.Applicative as Opt
import Wasp.Cli.Command (Command, CommandError (..), runCommand)
import qualified Wasp.Cli.Command.Call as Call
import Wasp.Cli.Command.Compile (defaultCompileOptions)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), WaspSpecAvailable (WaspSpecAvailable), require)
import Wasp.Cli.Command.Telemetry (runWithTelemetry)
import Wasp.Project (compileAndRenderDockerfile)

parserInfo :: Opt.ParserInfo (IO ())
parserInfo =
  Opt.info
    (pure $ runWithTelemetry Call.Other (runCommand printDockerfile))
    (Opt.progDesc "Print the contents of the Wasp-generated Dockerfile.")

printDockerfile :: Command ()
printDockerfile = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  dockerfileContentOrCompileErrors <- liftIO $ compileAndRenderDockerfile waspProjectDir (defaultCompileOptions waspProjectDir)
  either
    (throwError . CommandError "Displaying Dockerfile failed due to a compilation error in your Wasp project" . unwords)
    (liftIO . T.IO.putStrLn)
    dockerfileContentOrCompileErrors
