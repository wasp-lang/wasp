module Wasp.Cli.Command.Deps
  ( deps,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (toList)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Compile (defaultCompileOptions)
import Wasp.Cli.Terminal (title)
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.WebAppGenerator as WebAppGenerator
import Wasp.Lib (analyzeWaspProject)
import qualified Wasp.Util.Terminal as Term

deps :: Command ()
deps = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  (_, appSpecOrCompileErrors) <- liftIO $ analyzeWaspProject waspProjectDir (defaultCompileOptions waspProjectDir)
  appSpec <-
    either
      (throwError . CommandError "Determining dependencies failed due to a compilation error in your Wasp project" . unwords . toList)
      return
      appSpecOrCompileErrors

  liftIO . putStrLn $ depsMessage appSpec

depsMessage :: AppSpec -> String
depsMessage appSpec =
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
        ( N.waspDependencies $ WebAppGenerator.npmDepsForWasp appSpec
        )
      ++ [""]
      ++ printDeps
        "Webapp devDependencies:"
        ( N.waspDevDependencies $ WebAppGenerator.npmDepsForWasp appSpec
        )

printDeps :: String -> [AS.Dependency.Dependency] -> [String]
printDeps dependenciesTitle dependencies =
  title dependenciesTitle : map printDep dependencies

printDep :: AS.Dependency.Dependency -> String
printDep dep =
  Term.applyStyles [Term.Cyan] (AS.Dependency.name dep)
    ++ "@"
    ++ Term.applyStyles [Term.Yellow] (AS.Dependency.version dep)
