module Wasp.Cli.Command.Install
  ( install,
  )
where

import Control.Monad.Except (throwError)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Util.Terminal (styleCode)

-- | Wasp 0.27 dropped `wasp install`, because `npm install` installs Wasp now.
-- This stub tells the scripts that still call it what to run instead.
-- TODO: Remove it in Wasp 0.28.
install :: Command ()
install =
  throwError
    $ CommandError "The `wasp install` command was removed"
    $ "Run " ++ styleCode "npm install" ++ " instead."
