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
import Wasp.Generator.ServerGenerator.JobGenerator (pgBossVersionBounds)
import qualified Wasp.Generator.WebAppGenerator as WebAppGenerator
import qualified Wasp.Util.Terminal as Term

-- TODO: How to handle conditional includes that depend AppSpec?
-- Example: pg-boss but only when `jobs` are used.
-- Is the pattern below ok?
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
            ( N.waspDependencies $ ServerGenerator.npmDepsForWasp Nothing
            )
          ++ [printDep (AS.Dependency.Dependency "pg-boss@" pgBossVersionBounds) ++ " <-- Only if PgBoss is used in your jobs"]
          ++ [""]
          ++ printDeps
            "Server devDependencies:"
            ( N.waspDevDependencies $ ServerGenerator.npmDepsForWasp Nothing
            )
          ++ [""]
          ++ printDeps
            "Webapp dependencies:"
            ( N.waspDependencies WebAppGenerator.npmDepsForWasp
            )
          ++ [""]
          ++ printDeps
            "Webapp devDependencies:"
            ( N.waspDevDependencies WebAppGenerator.npmDepsForWasp
            )

printDeps :: String -> [AS.Dependency.Dependency] -> [String]
printDeps dependenciesTitle dependencies =
  title dependenciesTitle : map printDep dependencies

printDep :: AS.Dependency.Dependency -> String
printDep dep =
  Term.applyStyles [Term.Cyan] (AS.Dependency.name dep)
    ++ "@"
    ++ Term.applyStyles [Term.Yellow] (AS.Dependency.version dep)
