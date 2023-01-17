module Wasp.Cli.Command.Deploy
  ( deploy,
  )
where

import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import qualified Wasp.Lib as Lib

deploy :: [String] -> Command ()
deploy cmdArgs = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  liftIO $ Lib.deploy waspProjectDir cmdArgs
