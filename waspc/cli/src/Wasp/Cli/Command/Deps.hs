module Wasp.Cli.Command.Deps
  ( deps,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Compile (defaultCompileOptions)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), WaspSpecAvailable (WaspSpecAvailable), require)
import Wasp.Cli.Terminal (title)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.WaspLibs.AvailableLibs as AvailableLibs
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import Wasp.Project (analyzeWaspProject)
import qualified Wasp.Util.Terminal as Term

deps :: Command ()
deps = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  (appSpecOrAnalyzerErrors, _analyzerWarnings) <-
    liftIO $ analyzeWaspProject waspProjectDir (defaultCompileOptions waspProjectDir)
  appSpec <-
    either
      (throwError . CommandError "Determining dependencies failed due to a compilation error in your Wasp project" . unwords)
      return
      appSpecOrAnalyzerErrors

  liftIO $ putStrLn $ depsMessage appSpec

depsMessage :: AppSpec -> String
depsMessage appSpec =
  unlines $
    [ "",
      title "Below are listed the dependencies that Wasp uses in your project. You can import and use these directly in the code as if you specified them yourself, but you can't change their versions.",
      ""
    ]
      ++ printDeps
        "Server dependencies:"
        (filterOutWaspLibDeps $ N.dependencies $ N.fromWasp $ ServerGenerator.npmDepsFromWasp appSpec)
      ++ [""]
      ++ printDeps
        "Server devDependencies:"
        (filterOutWaspLibDeps $ N.devDependencies $ N.fromWasp $ ServerGenerator.npmDepsFromWasp appSpec)
  where
    waspLibPackageNames = Set.fromList $ map WaspLib.packageName AvailableLibs.waspLibs
    filterOutWaspLibDeps = filter ((`Set.notMember` waspLibPackageNames) . Npm.Dependency.name)

printDeps :: String -> [Npm.Dependency.Dependency] -> [String]
printDeps dependenciesTitle dependencies =
  title dependenciesTitle : map printDep dependencies

printDep :: Npm.Dependency.Dependency -> String
printDep dep =
  Term.applyStyles [Term.Cyan] (Npm.Dependency.name dep)
    ++ "@"
    ++ Term.applyStyles [Term.Yellow] (Npm.Dependency.version dep)
