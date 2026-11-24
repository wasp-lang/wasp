module Wasp.Cli.Command.Dockerfile
  ( printDockerfile,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Wasp.Cli.Command (Command, commandError, getConsole, require)
import Wasp.Cli.Command.Compile (defaultCompileOptions)
import Wasp.Cli.Command.Message (cliWriteRawOutputC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Project (compileAndRenderDockerfile)
import Wasp.Util.IO.Process (OutputStream (..))

printDockerfile :: Command ()
printDockerfile = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  console <- getConsole
  dockerfileContentOrCompileErrors <- liftIO $ compileAndRenderDockerfile waspProjectDir (defaultCompileOptions console waspProjectDir)
  either
    (throwError . commandError "Displaying Dockerfile failed due to a compilation error in your Wasp project" . unwords)
    (cliWriteRawOutputC Stdout . (++ "\n") . T.unpack)
    dockerfileContentOrCompileErrors
