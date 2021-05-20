module Command.Deps
  ( deps,
  )
where

import Cli.Terminal (title)
import Command (Command)
import Control.Monad.IO.Class (liftIO)
import qualified Generator.ServerGenerator as ServerGenerator
import qualified Generator.WebAppGenerator as WebAppGenerator
import NpmDependency (printDep)

deps :: Command ()
deps =
  liftIO $
    putStrLn $
      unlines $
        [ "",
          title "Below are listed the dependencies that Wasp uses in your project. You can import and use these directly in the code as if you specified them yourself, but you can't change their versions.",
          "",
          title "Server dependencies:"
        ]
          ++ map printDep ServerGenerator.waspNpmDeps
          ++ [ "",
               title "Webapp dependencies:"
             ]
          ++ map printDep WebAppGenerator.waspNpmDeps
