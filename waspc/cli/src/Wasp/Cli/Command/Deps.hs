module Wasp.Cli.Command.Deps
  ( deps,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Compile (compileOptions)
import Wasp.Cli.Terminal (title)
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.WebAppGenerator as WebAppGenerator
import Wasp.Lib (makeAppSpec)
import qualified Wasp.Util.Terminal as Term

deps :: Command ()
deps = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  maybeAppSpec <- liftIO $ makeAppSpec waspProjectDir (compileOptions waspProjectDir)
  case maybeAppSpec of
    Left compileErrors -> throwError $ CommandError "Determing dependencies failed" (unwords compileErrors)
    Right appSpec ->
      liftIO $
        putStrLn $
          unlines $
            [ "",
              title "Below are listed the dependencies that Wasp uses in your project. You can import and use these directly in the code as if you specified them yourself, but you can't change their versions.",
              ""
            ]
              ++ printDeps
                "Server dependencies:"
                ( N.waspDependencies $ ServerGenerator.npmDepsForWasp appSpec
                )
              ++ [""]
              ++ printDeps
                "Server devDependencies:"
                ( N.waspDevDependencies $ ServerGenerator.npmDepsForWasp appSpec
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
