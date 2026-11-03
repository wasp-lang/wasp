module Wasp.Cli.Command.Deps
  ( deps,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (defaultCompileOptions)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspNpmPackageAvailable (WaspNpmPackageAvailable (WaspNpmPackageAvailable))
import Wasp.Cli.Terminal (title)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.WaspLibs.AvailableLibs as AvailableLibs
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import qualified Wasp.Generator.WaspNpmPackagePeerDeps as WaspNpmPackagePeerDeps
import Wasp.Project (analyzeWaspProject)
import qualified Wasp.Util.Terminal as Term

deps :: Command ()
deps = do
  InWaspProject waspProjectDir <- require
  WaspNpmPackageAvailable <- require
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
      title "Below are the dependencies Wasp uses in your project. You can import them in your code as if you listed them yourself. To use another version, see https://wasp.sh/docs/advanced/dependencies#overriding-wasps-dependencies-advanced.",
      ""
    ]
      ++ printDeps
        "Peer dependencies of @wasp/wasp:"
        WaspNpmPackagePeerDeps.waspNpmPackagePeerDeps
      ++ [""]
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
