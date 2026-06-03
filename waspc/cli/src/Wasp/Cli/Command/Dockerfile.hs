module Wasp.Cli.Command.Dockerfile
  ( printDockerfile,
    parserInfo,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as T.IO
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Compile (defaultCompileOptions)
import Wasp.Cli.Command.Definition (CommandParserInfo, command)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), WaspSpecAvailable (WaspSpecAvailable), require)
import Wasp.Project (compileAndRenderDockerfile)

parserInfo :: CommandParserInfo
parserInfo = command "Print the contents of the Wasp-generated Dockerfile." printDockerfile

printDockerfile :: Command ()
printDockerfile = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  dockerfileContentOrCompileErrors <- liftIO $ compileAndRenderDockerfile waspProjectDir (defaultCompileOptions waspProjectDir)
  either
    (throwError . CommandError "Displaying Dockerfile failed due to a compilation error in your Wasp project" . unwords)
    (liftIO . T.IO.putStrLn)
    dockerfileContentOrCompileErrors
