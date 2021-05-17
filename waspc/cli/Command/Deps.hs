module Command.Deps
  ( deps,
  )
where

import Command (Command)
import Control.Monad.IO.Class (liftIO)
import Generator.ServerGenerator as Server (waspNpmDeps)
import qualified Generator.WebAppGenerator as WebApp (waspNpmDeps)
import NpmDependency (printDep)
import qualified Util.Terminal as Term

deps :: Command ()
deps =
  liftIO $
    putStrLn $
      unlines $
        [ "",
          title "WASP DEPENDENCIES",
          title "Server dependencies:"
        ]
          ++ map printDep Server.waspNpmDeps
          ++ ["", title "Webapp dependencies:"]
          ++ map printDep WebApp.waspNpmDeps

title :: String -> String
title = Term.applyStyles [Term.Bold]
