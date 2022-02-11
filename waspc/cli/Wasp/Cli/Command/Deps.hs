module Wasp.Cli.Command.Deps
  ( deps,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.Cli.Command (Command)
import Wasp.Cli.Terminal (title)
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.WebAppGenerator as WebAppGenerator
import qualified Wasp.Util.Terminal as Term

deps :: Command ()
deps =
  liftIO $
    putStrLn $
      unlines $
        [ "",
          title "Below are listed the dependencies that Wasp uses in your project. You can import and use these directly in the code as if you specified them yourself, but you can't change their versions.",
          ""
        ]
          ++ printDeps
            "Server dependencies:"
            ( N.dependencies ServerGenerator.waspNpmDependencies
            )
          ++ [""]
          ++ printDeps
            "Server devDependencies:"
            ( N.devDependencies ServerGenerator.waspNpmDependencies
            )
          ++ [""]
          ++ printDeps
            "Webapp dependencies:"
            ( N.dependencies WebAppGenerator.waspNpmDependencies
            )
          ++ [""]
          ++ printDeps
            "Webapp devDependencies:"
            ( N.devDependencies WebAppGenerator.waspNpmDependencies
            )

printDeps :: String -> [AS.Dependency.Dependency] -> [String]
printDeps dependenciesTitle dependencies =
  title dependenciesTitle : map printDep dependencies

printDep :: AS.Dependency.Dependency -> String
printDep dep =
  Term.applyStyles [Term.Cyan] (AS.Dependency.name dep)
    ++ "@"
    ++ Term.applyStyles [Term.Yellow] (AS.Dependency.version dep)
