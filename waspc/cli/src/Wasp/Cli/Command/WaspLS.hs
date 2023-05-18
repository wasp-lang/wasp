module Wasp.Cli.Command.WaspLS
  ( runWaspLS,
  )
where

import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (WaspLSArgs (..))
import qualified Wasp.LSP.Server as LS

runWaspLS :: WaspLSArgs -> Command ()
runWaspLS WaspLSArgs {wslLogFile = lf, wslUseStudio = _} = liftIO $ LS.serve lf
