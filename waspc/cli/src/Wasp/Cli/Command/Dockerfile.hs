module Wasp.Cli.Command.Dockerfile
  ( printDockerfile,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as T.IO
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Compile (defaultCompileOptions)
import Wasp.Lib (compileAndRenderDockerfile)

printDockerfile :: Command ()
printDockerfile = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  dockerfileContentOrCompileErrors <- liftIO $ compileAndRenderDockerfile waspProjectDir (defaultCompileOptions waspProjectDir)
  either
    (throwError . CommandError "Displaying Dockerfile failed due to a compilation error in your Wasp project" . unwords)
    (liftIO . T.IO.putStrLn)
    dockerfileContentOrCompileErrors
