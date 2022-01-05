module Wasp.Cli.Command.Deps
  ( deps,
  )
where

import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Terminal (title)
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.WebAppGenerator as WebAppGenerator
import Wasp.NpmDependency (printDep)

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
          ++ map printDep (error "TODO: WebAppGenerator.waspNpmDeps")
