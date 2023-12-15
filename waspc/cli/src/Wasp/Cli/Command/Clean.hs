module Wasp.Cli.Command.Clean
  ( clean,
  )
where

import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (deleteDotWaspDirIfExists)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)

clean :: Command ()
clean = do
  InWaspProject waspProjectDir <- require
  deleteDotWaspDirIfExists waspProjectDir
